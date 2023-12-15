use thin_vec::ThinVec;

#[derive(Clone, Copy, Debug)]
pub enum ValueType {
    Number,
    Point,
    ListOfPoint,
    ListOfNum,
}

#[derive(Clone, Debug)]
pub struct FnType {
    output: ValueType,
    params: ThinVec<ValueType>,
}
