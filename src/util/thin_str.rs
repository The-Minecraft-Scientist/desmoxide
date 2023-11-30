use std::{alloc::Layout, marker::PhantomData, ops::Deref};

#[derive(Debug, Clone)]
pub struct ThinStr<'a> {
    ptr: *mut ThinStrInner<'a>,
}

impl<'a> Deref for ThinStr<'a> {
    type Target = str;
    fn deref(&self) -> &Self::Target {
        unsafe {
            let inner = *self.ptr;
            std::str::from_utf8_unchecked(std::slice::from_raw_parts(inner.bytes, inner.len))
        }
    }
}
impl<'a> From<&'a str> for ThinStr<'a> {
    fn from(value: &'a str) -> Self {
        let inner = ThinStrInner {
            len: value.len(),
            bytes: value.as_bytes().as_ptr(),
            _phantom: PhantomData,
        };
        unsafe {
            let alloced = std::alloc::alloc(ThinStrInner::LAYOUT);
            core::ptr::write(core::mem::transmute(alloced), inner);
            ThinStr {
                ptr: alloced as *mut ThinStrInner,
            }
        }
    }
}
impl<'a> Drop for ThinStr<'a> {
    fn drop(&mut self) {
        unsafe { std::alloc::dealloc(self.ptr as *mut u8, ThinStrInner::LAYOUT) }
    }
}
#[derive(Debug)]
struct ThinStrInner<'a> {
    len: usize,
    bytes: *const u8,
    _phantom: PhantomData<&'a u8>,
}
impl<'a> ThinStrInner<'a> {
    const LAYOUT: Layout = core::alloc::Layout::new::<Self>();
}
