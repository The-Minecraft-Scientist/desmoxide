use super::lexer::Opcode;
pub const INDEX: Opcode = Opcode::newl(0);
pub const EQ: Opcode = Opcode::newl(0);
pub const ADD: Opcode = Opcode::newl(10);
pub const SUB: Opcode = Opcode::newl(10);
pub const MUL: Opcode = Opcode::newl(20);
pub const DIV: Opcode = Opcode::newl(20);
pub const POW: Opcode = Opcode::newr(30);
