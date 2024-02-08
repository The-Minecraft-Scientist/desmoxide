use super::disjoint_set::UsizeDisjointSet;

pub struct EGraph<Term> {
    //Union-find datastructure. Maps E-node (operation) Ids to the "canonical" (representative) Id
    classes: UsizeDisjointSet<Term>,
}
pub trait TermIntern<Term> {
    fn map(t: Term) -> usize
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
