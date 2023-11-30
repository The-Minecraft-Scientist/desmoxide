use std::{alloc::Layout, fmt::Debug, marker::PhantomData, ops::Deref};

pub struct ThinStr<'a> {
    ptr: *mut ThinStrInner<'a>,
}
impl<'a> ThinStr<'a> {
    pub fn new(value: &'a str) -> Self {
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
    pub fn as_str(&self) -> &'a str {
        unsafe {
            let inner = *self.ptr;
            std::str::from_utf8_unchecked(std::slice::from_raw_parts(inner.bytes, inner.len))
        }
    }
}
impl<'a> Debug for ThinStr<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("\"{}\"", self.as_str()))?;
        Ok(())
    }
}
impl<'a> Clone for ThinStr<'a> {
    fn clone(&self) -> Self {
        unsafe {
            let alloced = std::alloc::alloc(ThinStrInner::LAYOUT);
            core::ptr::write(core::mem::transmute(alloced), *self.ptr);
            Self {
                ptr: alloced as *mut ThinStrInner,
            }
        }
    }
}
impl<'a> Deref for ThinStr<'a> {
    type Target = str;
    fn deref(&self) -> &Self::Target {
        self.as_str()
    }
}
impl<'a> From<&'a str> for ThinStr<'a> {
    fn from(value: &'a str) -> Self {
        ThinStr::new(value)
    }
}
impl<'a> Drop for ThinStr<'a> {
    fn drop(&mut self) {
        unsafe { std::alloc::dealloc(self.ptr as *mut u8, ThinStrInner::LAYOUT) }
    }
}
#[derive(Debug, Clone, Copy)]
struct ThinStrInner<'a> {
    len: usize,
    bytes: *const u8,
    _phantom: PhantomData<&'a u8>,
}
impl<'a> ThinStrInner<'a> {
    const LAYOUT: Layout = core::alloc::Layout::new::<Self>();
}
