use std::{
    cell::{Cell, RefCell},
    collections::HashMap,
    fmt::Debug,
    hash::Hash,
    rc::Rc,
};

use nohash_hasher::IntMap;

use crate::impl_trivial_conversion;

use super::disjoint_set::UsizeDisjointSet;

#[derive(Debug, Clone)]
pub struct EGraph<T> {
    //Union-find datastructure. Maps E-node (operation) Ids to the "canonical" (representative) Id
    class_equivalences: UsizeDisjointSet<Id>,
    term_memo: TermMemoizer<T>,
    term_classes: IntMap<TermId, Id>,
    classes: IntMap<Id, EClass>,
}
#[derive(Debug, Clone)]
pub struct TermMemoizer<T> {
    seen_terms: RefCell<HashMap<T, TermId>>,
}
impl<T: Term> TermMemoizer<T> {
    pub fn add(&mut self, term: T) -> TermId {
        match self.seen_terms.borrow_mut().entry(term) {
            std::collections::hash_map::Entry::Occupied(o) => todo!(),
            std::collections::hash_map::Entry::Vacant(v) => todo!(),
        }

        todo!()
    }
}
impl<T> EGraph<T>
where
    T: Term,
    T::Symbol: From<usize> + Into<usize> + Copy + Clone + PartialEq + Eq,
{
    //Canonicalize all argument references from a Term
    pub fn canonicalize(&self, t: &T) {
        t.args()
            .iter()
            .for_each(|c| c.set(self.class_equivalences.find(c.get())));
    }
    pub fn in_same_class(&self, t1: TermId, t2: TermId) -> bool {
        match (self.term_classes.get(&t1), self.term_classes.get(&t2)) {
            (Some(i1), Some(i2)) => self.class_equivalences.are_eq(*i1, *i2),
            _ => false,
        }
    }
    //pub fn class_of(&self, t: &T) -> Option<Id> {}
}

pub trait Term: Debug + Clone + Hash + PartialEq + Eq {
    type Symbol;
    fn args(&self) -> &[Cell<Id>];
    fn sym(&self) -> Self::Symbol;
}
#[derive(Debug, Clone)]
pub struct EClass {
    nodes: Vec<TermId>,
    parents: Vec<(TermId, Id)>,
}

#[derive(Debug, Clone, Copy)]
///E-Class ID
pub struct Id(usize);
impl Hash for Id {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        state.write_usize(self.0)
    }
}
impl nohash_hasher::IsEnabled for Id {}
impl_trivial_conversion!(Id, usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
///Term ID
pub struct TermId(usize);
impl Hash for TermId {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        state.write_usize(self.0)
    }
}
impl nohash_hasher::IsEnabled for TermId {}
impl_trivial_conversion!(TermId, usize);
