use std::collections::HashMap;

use crate::ast::{Ident};

use super::ir::Id;

pub struct IdentMap<'source> {
    inner: HashMap<Ident<'source>, Option<Id>>,
}
