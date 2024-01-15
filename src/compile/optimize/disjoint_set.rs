use std::{
    cell::Cell,
    marker::PhantomData,
    mem,
    num::{NonZeroU32, NonZeroUsize},
};

use crate::util::Discard;

#[derive(Debug, Clone)]
pub struct DisjointSetInner {
    parents: Vec<Cell<u32>>,
    ranks: Vec<Cell<u8>>,
}
impl DisjointSetInner {
    #[inline]
    pub unsafe fn parent_of_unchecked(&self, elem: u32) -> u32 {
        self.parents.get_unchecked(elem as usize).get()
    }
    #[inline]
    pub fn parent_of(&self, elem: u32) -> u32 {
        self.parents
            .get(elem as usize)
            .expect("parent_of indexed out of bounds")
            .get()
    }
    #[inline]
    pub fn rank_of(&self, elem: u32) -> &Cell<u8> {
        self.ranks
            .get(elem as usize)
            .expect("rank_of indexed out of bounds")
    }
    #[inline]
    pub unsafe fn rank_of_unchecked(&self, elem: u32) -> &Cell<u8> {
        self.ranks.get_unchecked(elem as usize)
    }
    #[inline]
    pub fn set_parent(&self, elem: u32, parent: u32) {
        self.parents.get(elem as usize).unwrap().set(parent);
    }
    #[inline]
    pub unsafe fn set_parent_unchecked(&self, elem: u32, parent: u32) {
        self.parents.get_unchecked(elem as usize).set(parent);
    }

    #[inline]
    pub fn find(&self, mut elem: u32) -> u32 {
        let mut parent = self.parent_of(elem);
        if elem == parent {
            return elem;
        }
        //SAFETY: parent came from our parents vec, not the user, so it's guaranteed to be in-bounds
        let mut grandparent = unsafe { self.parent_of_unchecked(parent) };
        while parent != grandparent {
            //SAFETY: all of these values are sourced from within the internal vec, and all parent indices placed in the parents vec are in-bounds
            unsafe { self.set_parent_unchecked(elem, grandparent) }
            elem = parent;
            parent = grandparent;
            //SAFETY: see above
            grandparent = unsafe { self.parent_of_unchecked(parent) }
        }
        parent
    }
    #[inline]
    pub unsafe fn find_unchecked(&self, mut elem: u32) -> u32 {
        //SAFETY: caller guarantees elem is in-bounds
        let mut parent = self.parent_of_unchecked(elem);
        if elem == parent {
            return elem;
        }
        //SAFETY: parent came from our parents vec, not the user, so it's guaranteed to be in-bounds
        let mut grandparent = self.parent_of_unchecked(parent);
        while parent != grandparent {
            //SAFETY: all of these values are sourced from within the internal vec, and all parent indices placed in the parents vec are in-bounds
            unsafe { self.set_parent_unchecked(elem, grandparent) }
            elem = parent;
            parent = grandparent;
            //SAFETY: see above
            grandparent = self.parent_of_unchecked(parent);
        }
        parent
    }
    #[inline]
    pub fn nodes_eq(&self, lhs: u32, rhs: u32) -> bool {
        self.find(lhs) == self.find(rhs)
    }
    #[inline]
    pub unsafe fn nodes_eq_unchecked(&self, lhs: u32, rhs: u32) -> bool {
        self.find_unchecked(lhs) == self.find_unchecked(rhs)
    }
    #[inline]
    pub fn join(&self, x: u32, y: u32) -> u32 {
        let mut a = self.find(x);
        let mut b = self.find(y);
        //SAFETY: self.find(x) guarantees its result is in-bounds
        let mut a_rank = unsafe { self.rank_of_unchecked(a) }.get();
        let mut b_rank = unsafe { self.rank_of_unchecked(b) }.get();
        if b_rank >= a_rank {
            mem::swap(&mut a, &mut b);
        }
        if a_rank == b_rank {
            unsafe { self.rank_of_unchecked(b) }.set(a_rank + 1);
        }
        //SAFETY: a and b are bounds-checked by parent_of
        unsafe { self.set_parent_unchecked(a, b) };
        b
    }
    #[inline]
    pub unsafe fn join_unchecked(&self, x: u32, y: u32) -> u32 {
        //SAFETY: caller guarantees x and y are in-bounds
        let mut a = self.find_unchecked(x);
        let mut b = self.find_unchecked(y);
        //SAFETY: self.find(x) guarantees its result is in-bounds
        let mut a_rank = unsafe { self.rank_of_unchecked(a) }.get();
        let mut b_rank = unsafe { self.rank_of_unchecked(b) }.get();
        if b_rank >= a_rank {
            mem::swap(&mut a, &mut b);
        }
        if a_rank == b_rank {
            unsafe { self.rank_of_unchecked(b) }.set(a_rank + 1);
        }
        //SAFETY: a and b are bounds-checked by parent_of
        unsafe { self.set_parent_unchecked(a, b) };
        b
    }
    pub fn new() -> Self {
        Self {
            parents: Vec::new(),
            ranks: Vec::new(),
        }
    }
    pub fn new_elem(&mut self) -> u32 {
        let i = self
            .parents
            .len()
            .try_into()
            .expect("overflow while adding subtree to DisjointSet");
        self.parents.push(Cell::new(i));
        self.ranks.push(Cell::new(0));
        i
    }
}
