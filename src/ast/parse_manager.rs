use std::{
    cell::RefCell,
    collections::{hash_map::Entry, HashMap},
    fmt::{Debug, Formatter},
    num::NonZeroUsize,
    ops::{Deref, DerefMut, Index},
    slice::SliceIndex,
};

use debug_tree::{
    add_branch, add_branch_to, add_leaf, defer_write, scoped_branch::ScopedBranch, AsTree,
    TreeBuilder, TreeConfig, TreeSymbols,
};
use logos::{Lexer, Logos};
use thin_vec::{thin_vec, ThinVec};

use anyhow::{bail, Context, Result};

use super::{
    expression::{EquationType, ExpressionMeta, ExpressionType},
    ASTNode, ASTNodeRef,
    CoordinateAccess::*,
    Ident, ListCompInfo,
};
use crate::{
    assert_token_matches,
    ast::{BinaryOp, List, ListOp, Opcode, PiecewiseEntry, UnaryOp, Value},
    bad_token,
    lexer::Token,
    util::{multipeek::MultiPeek, LexIter},
};
#[derive(Clone)]
pub struct AST<'source> {
    pub store: Vec<ASTNode<'source>>,
    pub root: Option<ASTNodeRef>,
}
impl<'source> AST<'source> {
    pub fn new() -> Self {
        Self {
            store: Vec::with_capacity(10),
            root: None,
        }
    }
    pub fn place(&mut self, node: ASTNode<'source>) -> ASTNodeRef {
        self.push(node);
        unsafe { ASTNodeRef(NonZeroUsize::new_unchecked(self.len())) }
    }
    pub fn get_node(&self, idx: ASTNodeRef) -> Result<&ASTNode<'source>> {
        self.get(idx.0.get() as usize - 1)
            .context("invalid AST node reference")
    }
    pub fn get_node_mut(&mut self, idx: ASTNodeRef) -> Result<&mut ASTNode<'source>> {
        self.get_mut(idx.0.get() as usize - 1)
            .context("invalid AST node reference")
    }
    pub fn place_root(&mut self, root: ASTNode<'source>) {
        self.root = Some(self.place(root));
    }
    pub fn recursive_dbg(&self, builder: &mut TreeBuilder, nid: ASTNodeRef) -> Result<()> {
        macro_rules! named_branch {
            ($bname:expr, $bctx:expr,$($child:expr),+) => {
                {let b = builder.add_branch(&format!("{}: {}", $bname, $bctx));
                $(
                self.recursive_dbg(builder, *$child)?;
                )*
                b}
            };
        }
        macro_rules! named_branch_list {
            ($bname:expr, $bctx:expr,$children:expr) => {{
                let b = builder.add_branch(&format!("{}: {}", $bname, $bctx));
                for child in $children {
                    self.recursive_dbg(builder, *child)?;
                }
                b
            }};
        }

        let n = self.get_node(nid)?;
        if let ASTNode::Val(v) = n {
            builder.add_leaf(&format!("{:?}", v));
            return Ok(());
        }
        let name = n.as_ref();
        let s = match n {
            ASTNode::Binary(a, b, c) => named_branch!(name, c.as_ref(), a, b),
            ASTNode::Unary(a, c) => named_branch!(name, c.as_ref(), a),
            ASTNode::Parens(i, a) => named_branch!(name, i.0.as_str(), a),
            ASTNode::FunctionCall(i, r) => named_branch_list!(name, i.0.as_str(), r),
            ASTNode::Index(r, v) => named_branch!(name, "", r, v),
            ASTNode::List(l) => match l {
                List::List(v) => named_branch_list!(name, "", v),
                List::ListComp(a, info) => {
                    builder.add_branch("List Comprehension");
                    self.recursive_dbg(builder, *a);
                    named_branch_list!(name, "", info.vars.iter().map(|a| &a.1))
                }
                List::Range(a, b) => named_branch!("Range list", "", a, b),
            },
            ASTNode::ListFilt(l, a) => named_branch!(name, "", l, a),
            ASTNode::Point(a, b) => named_branch!(name, "", a, b),
            ASTNode::ListOp(a, o) => named_branch_list!(name, o.as_ref(), a),
            ASTNode::CoordinateAccess(a, b) => named_branch!(name, b.as_ref(), a),
            ASTNode::Comparison(a, c, ba) => {
                let mut b = builder.add_branch("Comparison");
                self.recursive_dbg(builder, *a);
                builder.add_leaf(c.as_ref());
                self.recursive_dbg(builder, *ba);
                b
            }
            ASTNode::Piecewise { default, entries } => {
                let b = builder.add_branch("if");
                for entry in entries {
                    let mut b1 = builder.add_branch("if");
                    self.recursive_dbg(builder, entry.comp)?;
                    let mut b0 = builder.add_branch("then");
                    self.recursive_dbg(builder, entry.result)?;
                    b0.release();
                    b1.release();
                }
                b
            }
            t => {
                bail!("incorrect AST node")
            }
        };
        Ok(())
    }
}
impl<'source> Index<ASTNodeRef> for AST<'source> {
    type Output = ASTNode<'source>;
    fn index(&self, index: ASTNodeRef) -> &Self::Output {
        &self.store[index.0.get() as usize - 1]
    }
}
impl<'source> Debug for AST<'source> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut builder = TreeBuilder::new();
        builder.set_config_override(TreeConfig::new().symbols(TreeSymbols::with_rounded()));
        self.recursive_dbg(&mut builder, self.root.unwrap())
            .unwrap();
        f.write_str(&builder.as_tree().string());
        Ok(())
    }
}
impl<'a> Deref for AST<'a> {
    type Target = Vec<ASTNode<'a>>;
    fn deref(&self) -> &Self::Target {
        &self.store
    }
}
impl<'a> DerefMut for AST<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.store
    }
}
pub struct ParseManager<'source> {
    pub lexer: MultiPeek<LexIter<'source, Token>>,
    pub ast: AST<'source>,
}
impl<'a> ParseManager<'a> {
    pub fn new(lex: MultiPeek<LexIter<'a, Token>>) -> Self {
        Self {
            lexer: lex,
            ast: AST::new(),
        }
    }
    pub fn parse(&mut self) -> Result<()> {
        let root = self.parse_expr(0)?;
        self.ast.place_root(root);
        Ok(())
    }
    pub fn split(self) -> (AST<'a>, MultiPeek<LexIter<'a, Token>>) {
        (self.ast, self.lexer)
    }

    fn place(&mut self, node: ASTNode<'a>) -> ASTNodeRef {
        self.ast.place(node)
    }
    fn parse_list_body(&mut self) -> Result<List<'a>> {
        let next_token = self.lexer.peek_next().context("unexpected EOF")?;
        //Empty list
        if next_token.1 == Token::RBracket {
            self.lexer.discard()?;
            //empty list
            return Ok(List::List(ThinVec::new()));
        }
        let first_scope = self.parse_expr(0)?;
        let next_token = self.lexer.peek_next().context("unexpected EOF")?;
        //Range list ([0...3])
        if let ASTNode::List(l) = first_scope {
            assert_token_matches!(self.lexer, Token::RBracket);
            return Ok(l.clone());
        }
        let mut vars = ThinVec::with_capacity(10);
        vars.push(self.place(first_scope));
        Ok(match next_token.1 {
            Token::Comma => {
                loop {
                    match self.lexer.next_res()? {
                        //We reached the last item
                        (_, Token::RBracket) => break,
                        // Normal, continue to the next item
                        (_, Token::Comma) => {}
                        (s, t) => {
                            bad_token!(s, t, "parsing list")
                        }
                    }
                    vars.push(self.parse_placed(0)?);
                }
                List::List(vars)
            }
            //Single element list
            Token::RBracket => {
                assert_token_matches!(self.lexer, Token::RBracket);
                List::List(vars)
            }
            t => {
                bad_token!(next_token.0, t, "parsing list")
            }
        })
    }
    fn parse_function_call(&mut self) -> Result<ThinVec<ASTNodeRef>> {
        assert_token_matches!(self.lexer, Token::LParen);
        self.parse_fn_call(ThinVec::with_capacity(5))
    }
    //Sepcial case for when suffix call has already introduced an initial argument
    fn parse_fn_call(&mut self, vars: ThinVec<ASTNodeRef>) -> Result<ThinVec<ASTNodeRef>> {
        self.parse_values(vars, Token::RParen)
    }
    //generic function to parse a comma-seperated list of AST nodes concluded by the token "tok"
    fn parse_values(
        &mut self,
        mut vars: ThinVec<ASTNodeRef>,
        tok: Token,
    ) -> Result<ThinVec<ASTNodeRef>> {
        let next_token = self.lexer.peek_next().context("unexpected EOF")?;
        //Empty list
        if next_token.1 == tok {
            self.lexer.discard()?;
            //empty list
            return Ok(vars);
        }
        let first_scope = self.parse_expr(0)?;
        let next_token = self.lexer.peek_next().context("unexpected EOF")?;
        //Range list ([0...3])
        if let ASTNode::List(ref _l) = first_scope {
            if next_token.1 == tok {
                return Ok(vars);
            }
        }
        vars.push(self.ast.place(first_scope));

        Ok(match next_token.1 {
            Token::Comma => {
                loop {
                    match self.lexer.next_res()? {
                        //We reached the last item
                        (_, Token::RParen) => break,
                        // Normal, continue to the next item
                        (_, Token::Comma) => {}
                        (s, t) => {
                            bad_token!(s, t, "parsing list")
                        }
                    }
                    vars.push(self.parse_placed(0)?)
                }
                vars
            }
            //Single element list
            t if t == tok => vars,
            t => {
                bad_token!(next_token.0, t, "parsing function arguments")
            }
        })
    }
    pub fn parse_placed(&mut self, min_bp: u8) -> Result<ASTNodeRef> {
        let s = self.parse_expr(min_bp)?;
        Ok(self.place(s))
    }
    pub fn parse_expr(&mut self, min_binding_power: u8) -> Result<ASTNode<'a>> {
        //get LHS token
        let next = self.lexer.next_res()?;

        let mut lhs = match next.1 {
            //Identifier
            Token::Ident => ASTNode::Val(next.0.into()),
            //Static value
            Token::IntegerLit(i) => ASTNode::Val(i.into()),
            Token::FloatLit(f) => ASTNode::Val(f.into()),
            //Prefix negation
            Token::Minus => ASTNode::Unary(
                self.parse_placed(*Opcode::Neg.prefix_bp()?)?,
                super::UnaryOp::Neg,
            ),
            // TODO: piecewise functions
            Token::LGroup => {
                if self.lexer.peek_next_res()?.1 == Token::RGroup {
                    self.lexer.discard()?;
                    ASTNode::Val(Value::ConstantI64(1)).into()
                } else {
                    let mut v = ThinVec::with_capacity(2);
                    let left = self.parse_expr(0)?;
                    let n = self.lexer.next_res()?;
                    if !n.1.is_comparison() {
                        bail!("Piecewises must have at least one condition");
                    }

                    let right = self.parse_expr(0)?;
                    assert_token_matches!(self.lexer, Token::Colon);
                    let result = self.parse_expr(0)?;
                    let n = ASTNode::Comparison(
                        self.place(left),
                        n.1.as_comparison()?,
                        self.place(right),
                    );
                    v.push(PiecewiseEntry {
                        comp: self.place(n),
                        result: self.place(result),
                    });
                    loop {
                        match self.lexer.next_res()?.1 {
                            Token::Comma => {
                                let s = self.parse_placed(0)?;
                                match self.lexer.peek_next_res()?.1 {
                                    Token::RGroup => {
                                        break ASTNode::Piecewise {
                                            default: s,
                                            entries: v,
                                        };
                                    }
                                    t if t.is_comparison() => {
                                        let right = self.parse_expr(0)?;
                                        let result =
                                            if self.lexer.peek_next_res()?.1 == Token::Comma {
                                                ASTNode::Val(Value::ConstantI64(1)).into()
                                            } else {
                                                self.parse_expr(0)?
                                            };
                                        let comp = ASTNode::Comparison(
                                            s,
                                            t.as_comparison()?,
                                            self.place(right),
                                        );
                                        v.push(PiecewiseEntry {
                                            comp: self.place(comp),
                                            result: self.place(result),
                                        });
                                    }
                                    t => bad_token!("", t, "while parsing piecewise"),
                                }
                            }
                            t => bad_token!("", t, "expected comma"),
                        }
                    }
                    .into()
                }
            }
            Token::LParen => {
                let ret = self.parse_expr(0)?;
                assert_token_matches!(self.lexer, Token::RParen);
                ret
            }
            Token::Frac => {
                //Handle latex frac command (\frac{numerator}{denominator})
                assert_token_matches!(self.lexer, Token::LGroup);
                let numerator = self.parse_placed(0)?;
                assert_token_matches!(self.lexer, Token::RGroup);
                assert_token_matches!(self.lexer, Token::LGroup);
                let denominator = self.parse_placed(0)?;
                assert_token_matches!(self.lexer, Token::RGroup);
                ASTNode::Binary(numerator, denominator, BinaryOp::Div).into()
            }
            Token::Sqrt => {
                match self.lexer.next_res()? {
                    //handle nthroot
                    (_, Token::LBracket) => {
                        let n = self.parse_placed(0)?;
                        assert_token_matches!(self.lexer, Token::RBracket);
                        assert_token_matches!(self.lexer, Token::LGroup);
                        let base = self.parse_placed(0)?;
                        assert_token_matches!(self.lexer, Token::RGroup);
                        ASTNode::Binary(n, base, BinaryOp::NthRoot).into()
                    }
                    //handle pure sqrt
                    (_, Token::LGroup) => {
                        let base = self.parse_placed(0)?;
                        assert_token_matches!(self.lexer, Token::RGroup);
                        ASTNode::Unary(base, UnaryOp::Sqrt)
                    }
                    (s, t) => bad_token!(s, t, "in sqrt expression"),
                }
            }
            Token::LBracket => ASTNode::List(self.parse_list_body()?).into(),
            Token::Random => {
                assert_token_matches!(self.lexer, Token::LParen);
                if self.lexer.peek_next_res()?.1 == Token::RParen {
                    ASTNode::ListOp(thin_vec![], ListOp::Random).into()
                } else {
                    let mut v = ThinVec::with_capacity(2);
                    v.push(self.parse_placed(0)?);
                    match self.lexer.peek_next_res()? {
                        (_, Token::Comma) => {
                            self.lexer.catch_up();
                            v.push(self.parse_placed(0)?);
                        }
                        (_, Token::RParen) => {}
                        t => bad_token!(t.0, t.1, "parsing random() function args"),
                    };
                    assert_token_matches!(self.lexer, Token::RParen | Token::Comma);
                    ASTNode::ListOp(v, ListOp::Random).into()
                }
            }
            t if t.is_simple_unary() => {
                assert_token_matches!(self.lexer, Token::LParen);
                let inner = self.parse_placed(0)?;
                assert_token_matches!(self.lexer, Token::RParen);
                ASTNode::new_simple_with_node(t, inner)?
            }
            t if t.binary_builtin().is_some() => {
                assert_token_matches!(self.lexer, Token::LParen);
                let arg0 = self.parse_placed(0)?;
                assert_token_matches!(self.lexer, Token::Comma);
                let arg1 = self.parse_placed(0)?;
                assert_token_matches!(self.lexer, Token::RParen);
                ASTNode::new_simple_binary(t.binary_builtin().unwrap(), arg0, arg1)?
            }
            t if t.has_autojoin_semantics() => {
                let arg = self.parse_function_call()?;
                ASTNode::new_list_fn(t, arg)?
            }
            t => bad_token!(next.0, t, "getting lhs"),
        };

        //Loop across this level
        loop {
            let Some(a) = *self.lexer.peek_next() else {
                break;
            };
            let op = match a.1 {
                Token::Plus => Opcode::Add,
                Token::Minus => Opcode::Sub,
                Token::Mul => Opcode::Mul,
                Token::Pow => {
                    self.lexer.discard()?;
                    assert_token_matches!(self.lexer, Token::LGroup);
                    let exp = self.parse_placed(0)?;
                    assert_token_matches!(self.lexer, Token::RGroup);

                    lhs = ASTNode::Binary(self.place(lhs), exp, BinaryOp::Pow);
                    continue;
                }
                Token::LBracket => Opcode::Index,
                Token::LParen => Opcode::Parens,
                Token::Dot => {
                    self.lexer.discard()?;
                    let id = self.lexer.next_res()?;
                    if let Token::Ident = id.1 {
                        self.lexer.discard()?;
                        let a = match id.0 {
                            "x" => DotAccessX,
                            "y" => DotAccessY,
                            "z" => DotAccessZ,
                            s => {
                                bail!("unexpected ident \"{}\" in dot accessor!", s)
                            }
                        };
                        lhs = ASTNode::CoordinateAccess(self.place(lhs), a).into();
                        continue;
                    }
                    if !id.1.has_dot_call_semantics() {
                        bail!("cannot call builtin {:?} as a suffix", id.1)
                    }
                    // Handle list function calls in suffix position (e.g. [1,2,3].sort()   )
                    if id.1.has_autojoin_semantics() {
                        let mut vars = ThinVec::with_capacity(10);
                        vars.push(self.place(lhs));
                        lhs = ASTNode::new_list_fn(id.1, self.parse_fn_call(vars)?)?;
                        continue;
                    }

                    todo!("Not all dot functions yet implemented")
                }
                Token::Range => {
                    self.lexer.discard()?;
                    //Immediately return
                    return Ok(
                        ASTNode::List(List::Range(self.place(lhs), self.parse_placed(0)?)).into(),
                    );
                }
                //List comp
                Token::For => {
                    self.lexer.discard()?;
                    //LHS is the expression to be run
                    let mut vars: ThinVec<(Ident<'a>, ASTNodeRef)> = ThinVec::with_capacity(2);
                    loop {
                        let (id, Token::Ident) =
                            self.lexer.peek_next().context("unexpected EOF")?
                        else {
                            break;
                        };
                        self.lexer.discard()?;
                        assert_token_matches!(self.lexer, Token::Eq);
                        vars.push((id.into(), self.parse_placed(0)?));
                        if self.lexer.peek_next().context("unexpected EOF")?.1 != Token::Comma {
                            break;
                        } else {
                            self.lexer.discard()?;
                        }
                    }
                    lhs = ASTNode::List(List::ListComp(self.place(lhs), ListCompInfo { vars }))
                        .into();
                    continue;
                }
                t if t.ends_parse() => {
                    break;
                }
                _t => {
                    //This is super jank but we unconditionally pop the self.lexer every iteration so...
                    self.lexer.push(("*", Token::Mul));
                    Opcode::Mul
                }
                t => bad_token!(a.0, t, "getting opcode"),
            };
            if let Ok(bp) = op.postfix_bp() {
                if *bp < min_binding_power {
                    break;
                }
                self.lexer.next();
                match (op, &lhs) {
                    (Opcode::Parens, ASTNode::Val(Value::Ident(ref s))) => {
                        let rhs = self.parse_placed(0)?;
                        let tok = self.lexer.next_res()?;
                        match tok.1 {
                            Token::RParen => {
                                lhs = ASTNode::Parens(s.clone(), rhs).into();
                            }
                            Token::Comma => {
                                let mut v = ThinVec::with_capacity(10);
                                v.push(rhs);
                                let v = self.parse_fn_call(v)?;
                                lhs = ASTNode::FunctionCall(s.clone(), v).into();
                            }
                            t => {
                                bad_token!(tok.0, t, "while parsing function call arguments")
                            }
                        }
                    }
                    (Opcode::Parens, _a) => {
                        let rhs = self.parse_placed(0)?;
                        assert_token_matches!(self.lexer, Token::RParen);
                        lhs = ASTNode::Binary(self.place(lhs), rhs, BinaryOp::Mul).into();
                    }
                    (Opcode::Index, _node) => {
                        let rhs = self.parse_placed(0)?;
                        match self.lexer.next_res()?.1 {
                            Token::RBracket => lhs = ASTNode::Index(self.place(lhs), rhs).into(),
                            Token::Comma => {
                                let mut v = ThinVec::with_capacity(10);
                                v.push(rhs);
                                let args = self.parse_values(v, Token::RBracket)?;
                                lhs = ASTNode::Index(
                                    self.place(lhs),
                                    self.place(ASTNode::List(List::List(args))),
                                )
                                .into();
                                assert_token_matches!(self.lexer, Token::RBracket);
                            }
                            t if t.is_comparison() => {
                                let compcase = self.parse_placed(0)?;
                                let comp = t.as_comparison()?;

                                lhs = ASTNode::ListFilt(
                                    self.place(lhs),
                                    self.place(ASTNode::Comparison(rhs, comp, compcase)),
                                )
                                .into();
                                assert_token_matches!(self.lexer, Token::RBracket);
                            }
                            t => bad_token!("", t, "parsing list index args"),
                        }
                    }
                    t => bail!("bad opcode: {:?}", t),
                }
                continue;
            }
            if let Ok(bp) = op.infix_bp() {
                if bp.left < min_binding_power {
                    break;
                }
                let _ = self.lexer.next();
                let rhs = self.parse_expr(bp.right)?;

                lhs = ASTNode::new_simple_binary(op, self.place(lhs), self.place(rhs))?;
                continue;
            }
            break;
        }
        Ok(lhs)
    }
}
