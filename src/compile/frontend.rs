use std::collections::HashMap;

use crate::ast::{parser::Parser, Ident};

use super::ir::Id;

pub struct IdentMap<'source> {
    inner: HashMap<Ident<'source>, Option<Id>>,
}
