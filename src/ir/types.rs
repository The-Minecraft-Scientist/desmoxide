use thin_vec::ThinVec;

#[derive(Clone, Copy, Debug)]
pub enum ValueType {
    Number,
    Point,
    List(&'static Self),
}

#[derive(Clone, Debug)]
pub struct FnType {
    output: ValueType,
    params: ThinVec<ValueType>,
}
