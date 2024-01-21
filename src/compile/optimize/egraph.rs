use super::disjoint_set::UsizeDisjointSet;

pub struct EGraph {
    //Union-find datastructure. Maps E-node (operation) Ids to the "canonical" (representative) Id
    classes: UsizeDisjointSet<Id>,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Id(usize);
impl From<usize> for Id {
    fn from(value: usize) -> Self {
        Id(value)
    }
}
impl From<Id> for usize {
    fn from(value: Id) -> Self {
        value.0
    }
}
