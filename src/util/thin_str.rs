use core::slice;
use std::{
    fmt::Debug, fmt::Display, marker::PhantomData, num::NonZeroU16, ops::Deref, ptr::NonNull,
};

// INVARIANTS:
// if Boxed bit is set:
// * the string pointer must point to and have mutable permissions for an allocation
// * the string len must be at least one (for a minimum of four bytes in the allocation)
// * the allocation pointed to by ptr must end with [0xEF, 0xBF, 0xBF]
// If the Boxed bit is not set:
// * the string pointer must point to and have shared permissions for an allocation
// * the string len must be at most 2^15
pub struct ThinStr<'a>(NonNull<u8>, PhantomData<&'a str>);
impl<'a> Debug for ThinStr<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.as_slice())
    }
}
impl<'a> Display for ThinStr<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.as_slice())
    }
}
impl<'a> ThinStr<'a> {
    pub const PTR_MASK: usize = 0xFFFFFFFFFFFF;
    pub const BOXED_FLAG: usize = 0x1000000000000;
    pub const MAX_SLICE_LEN: usize = 0x7FFF;
    #[inline(always)]
    fn ptr(&self) -> *const u8 {
        (self.0.as_ptr() as usize & Self::PTR_MASK) as *const u8
    }
    #[inline(always)]
    fn is_boxed(&self) -> bool {
        (self.0.as_ptr() as usize & Self::BOXED_FLAG) != 0
    }
    #[inline(always)]
    fn len(&self) -> u16 {
        (self.0.as_ptr() as usize >> 49) as u16
    }
    #[inline(always)]
    pub fn from_slice(slice: &'a str) -> Self {
        if slice.len() == 0 {
            return Self(
                unsafe { NonNull::new_unchecked(0x1usize as *mut u8) },
                PhantomData,
            );
        }
        //box if len is too large
        if slice.len() > Self::MAX_SLICE_LEN {
            //Ensure exact capacity so we don't accidentally trigger a realloc/dealloc when we turn this into a boxed slice
            let mut v = Vec::with_capacity(slice.len() + 3);
            v.extend(slice.bytes());
            v.extend([0xEF, 0xBF, 0xBF]);
            let ptr = Box::into_raw(v.into_boxed_slice());
            let trunc: *mut u8 = ptr.cast();
            Self(
                unsafe { NonNull::new_unchecked((trunc as usize | Self::BOXED_FLAG) as *mut u8) },
                PhantomData,
            )
        }
        //Happy path :)
        else {
            let mut val = slice.len();
            val <<= 49;

            let ptr = slice.as_bytes().as_ptr() as usize;
            //This should never happen in practice, but it's probably worth having a "just in case" check here
            val |= ptr;
            #[cfg(debug_assertions)]
            {
                assert!(ptr <= Self::PTR_MASK);
                assert!(val & Self::BOXED_FLAG == 0);
            }

            Self(
                unsafe { NonNull::new_unchecked(val as *mut u8) },
                PhantomData,
            )
        }
    }
    #[inline(always)]
    pub fn as_slice(&self) -> &'a str {
        if self.0.as_ptr() as usize == 0x1 {
            return "";
        }
        let len: usize = if self.is_boxed() {
            let mut ptr = self.ptr();
            let mut len = 1usize;
            let mut accum = 0u32;
            loop {
                let nextbyte = unsafe { *ptr };
                ptr = unsafe { ptr.add(1) };
                //println!("{:#024x}", accum);
                accum <<= 8;
                accum |= nextbyte as u32;
                len += 1;
                if accum & 0xFFFFFF == 0xEFBFBF {
                    break;
                }
            }
            len.wrapping_sub(4)
        } else {
            self.len() as usize
        };
        let s = unsafe { slice::from_raw_parts(self.ptr(), len) };
        return unsafe { std::str::from_utf8_unchecked(s) };
    }

    //gets the *entire* allocation backing this ThinStr if it is boxed
    #[inline(always)]
    fn get_as_box_if_boxed(&self) -> Option<Box<[u8]>> {
        if self.is_boxed() {
            let mut ptr = self.ptr();
            let mut len = 1usize;
            let mut accum = 0u32;
            loop {
                let nextbyte = unsafe { *ptr };
                ptr = unsafe { ptr.add(1) };
                accum <<= 8;
                accum |= nextbyte as u32;
                len += 1;
                if accum & 0xFFFFFF == 0xEFBFBF {
                    break;
                }
            }
            len = len.wrapping_sub(1);
            Some(unsafe { Box::from_raw(slice::from_raw_parts_mut(self.ptr() as *mut u8, len)) })
        } else {
            return None;
        }
    }
}
impl<'a> Deref for ThinStr<'a> {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.as_slice()
    }
}
impl<'a> Drop for ThinStr<'a> {
    #[inline(always)]
    fn drop(&mut self) {
        // calls Box Dtor, deallocates
        let _ = self.get_as_box_if_boxed();
    }
}
impl<'a> Clone for ThinStr<'a> {
    #[inline(always)]
    fn clone(&self) -> Self {
        if self.is_boxed() {
            let mut ptr = self.ptr();
            let mut len = 1usize;
            let mut accum = 0u32;
            loop {
                let nextbyte = unsafe { *ptr };
                ptr = unsafe { ptr.add(1) };
                accum <<= 8;
                accum |= nextbyte as u32;
                len += 1;
                if accum & 0xFFFFFF == 0xEFBFBF {
                    break;
                }
            }
            len = len.wrapping_sub(1);
            let bytes = unsafe { slice::from_raw_parts(self.ptr(), len) }
                .to_vec()
                .into_boxed_slice();
            Self(
                unsafe {
                    NonNull::new_unchecked(
                        (Self::BOXED_FLAG | Box::into_raw(bytes).cast::<*const u8>() as usize)
                            as *mut u8,
                    )
                },
                PhantomData,
            )
        } else {
            Self(self.0, PhantomData)
        }
    }
}

#[cfg(not(target_pointer_width = "64"))]
pub const _: () = assert!(false, "Desmoxide is only designed for 64-bit systems");

impl<'a> From<&'a str> for ThinStr<'a> {
    #[inline(always)]
    fn from(value: &'a str) -> Self {
        Self::from_slice(value)
    }
}
