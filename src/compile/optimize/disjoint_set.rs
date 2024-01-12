use std::{num::{NonZeroU32, NonZeroUsize}, cell::Cell};

#[derive(Debug, Clone)]
pub struct DisjointSetInner {
    items: Vec<Elem>,
}
impl DisjointSetInner {
    pub fn with_capacity(cap: u32) -> Self {
        Self {
            items: (0..cap).map(|val| Elem { val, info: NodeInfo::new() }).collect::<Vec<_>>(),
        }
    }
    pub fn new_set(&mut self) -> Elem {
        self.
    }
    pub fn find(&self, elem: Elem) -> Elem {

    }
}
#[derive(Debug, Clone)]
pub struct Elem {
    val: u32,
    info: Cell<NodeInfo>,
}
#[derive(Debug, Clone, Copy)]
#[repr(transparent)]
pub struct NodeInfo(u32);
impl NodeInfo {
    pub fn new_with_parent(val: u32) -> Self {
        assert!(val | Self::PARENT_MASK == Self::PARENT_MASK);
        let p: &u32 = unsafe {std::ptr::null().as_ref().unwrap()};
        Self(val)
    }
    pub fn new() -> Self {
        Self(0)
    }
    const PARENT_MASK: u32 = 0x7FFFFFF;
    #[inline]
    pub fn parent_raw(&self) -> u32 {
        self.0 & Self::PARENT_MASK
    }
    #[inline]
    pub fn parent(&self) -> Option<NonZeroU32> {
        NonZeroU32::new(self.parent_raw())
    }
    #[inline]
    pub fn rank(&self) -> u8 {
        (self.0 >> 27) as u8
    }
    #[inline]
    pub fn set_rank(&mut self, rank: u8) {
        self.0 = self.parent_raw() | ((rank as u32) << 27)
    }
}
