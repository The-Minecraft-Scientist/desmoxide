use std::{cell::Cell, marker::PhantomData, mem};

#[derive(Debug, Clone)]
pub struct DisjointSetRaw {
    parents: Vec<Cell<usize>>,
    ranks: Vec<Cell<u8>>,
}
#[derive(Debug, Clone)]
pub struct UsizeDisjointSet<T> {
    set: DisjointSetRaw,
    phantom: PhantomData<T>,
}
impl<T> UsizeDisjointSet<T>
where
    T: From<usize>,
    usize: From<T>,
{
    #[inline]
    pub fn raw(&self) -> &DisjointSetRaw {
        &self.set
    }
    #[inline]
    pub fn raw_mut(&mut self) -> &mut DisjointSetRaw {
        &mut self.set
    }
    #[inline]
    pub fn new_class(&mut self) -> T {
        self.set.new_elem().into()
    }
    #[inline]
    pub fn find(&self, val: T) -> T {
        self.set.find(val.into()).into()
    }
    #[inline]
    pub unsafe fn find_unchecked(&self, val: T) -> T {
        self.set.find_unchecked(val.into()).into()
    }
    pub fn union(&self, val_a: T, val_b: T) -> T {
        self.set.join(val_a.into(), val_b.into()).into()
    }
    pub unsafe fn union_unchecked(&self, val_a: T, val_b: T) -> T {
        self.set.join_unchecked(val_a.into(), val_b.into()).into()
    }
    pub fn are_eq(&self, val_a: T, val_b: T) -> bool {
        self.set.nodes_eq(val_a.into(), val_b.into())
    }
    pub fn flatten(&self) {
        self.set.flatten()
    }
}

impl DisjointSetRaw {
    #[inline]
    pub unsafe fn parent_cell_unchecked(&self, elem: usize) -> &Cell<usize> {
        #[cfg(not(debug_assertions))]
        {
            self.parents.get_unchecked(elem as usize)
        }

        #[cfg(debug_assertions)]
        {
            self.parents
                .get(elem as usize)
                .expect("index out of bounds in parent_unchecked!!!! this is REALLY bad")
        }
    }
    #[inline]
    pub unsafe fn parent_unchecked(&self, elem: usize) -> usize {
        self.parent_cell_unchecked(elem).get()
    }
    pub fn flatten(&self) {
        for c in self.parents.iter() {
            //SAFETY: c.get() is in-bounds because we put it there
            c.set(unsafe { self.find_unchecked(c.get()) })
        }
    }
    #[inline]
    pub fn parent(&self, elem: usize) -> usize {
        self.parents
            .get(elem as usize)
            .expect("parent_of indexed out of bounds")
            .get()
    }
    #[inline]
    pub fn rank(&self, elem: usize) -> &Cell<u8> {
        self.ranks
            .get(elem as usize)
            .expect("rank_of indexed out of bounds")
    }
    #[inline]
    pub unsafe fn rank_unchecked(&self, elem: usize) -> &Cell<u8> {
        #[cfg(not(debug_assertions))]
        {
            self.ranks.get_unchecked(elem as usize)
        }

        #[cfg(debug_assertions)]
        {
            self.ranks
                .get(elem as usize)
                .expect("index out of bounds in rank_unchecked!!!! this is REALLY bad")
        }
    }
    #[inline]
    pub fn set_parent(&self, elem: usize, parent: usize) {
        self.parents.get(elem as usize).unwrap().set(parent);
    }
    #[inline]
    pub unsafe fn set_parent_unchecked(&self, elem: usize, parent: usize) {
        self.parent_cell_unchecked(elem).set(parent);
    }

    #[inline]
    pub fn find(&self, mut elem: usize) -> usize {
        let mut parent = self.parent(elem);
        if elem == parent {
            return elem;
        }
        //SAFETY: parent came from our parents vec, not the user, so it's guaranteed to be in-bounds
        let mut grandparent = unsafe { self.parent_unchecked(parent) };
        while parent != grandparent {
            //SAFETY: all of these values are sourced from within the internal vec, and all parent indices placed in the parents vec are in-bounds
            unsafe { self.set_parent_unchecked(elem, grandparent) }
            elem = parent;
            parent = grandparent;
            //SAFETY: see above
            grandparent = unsafe { self.parent_unchecked(parent) }
        }
        parent
    }
    #[inline]
    pub unsafe fn find_unchecked(&self, mut elem: usize) -> usize {
        //SAFETY: caller guarantees elem is in-bounds
        let mut parent = self.parent_unchecked(elem);
        if elem == parent {
            return elem;
        }
        //SAFETY: parent came from our parents vec, not the user, so it's guaranteed to be in-bounds
        let mut grandparent = self.parent_unchecked(parent);
        while parent != grandparent {
            //SAFETY: all of these values are sourced from within the internal vec, and all parent indices placed in the parents vec are in-bounds
            unsafe { self.set_parent_unchecked(elem, grandparent) }
            elem = parent;
            parent = grandparent;
            //SAFETY: see above
            grandparent = self.parent_unchecked(parent);
        }
        parent
    }
    #[inline]
    pub fn nodes_eq(&self, lhs: usize, rhs: usize) -> bool {
        self.find(lhs) == self.find(rhs)
    }
    #[inline]
    pub unsafe fn nodes_eq_unchecked(&self, lhs: usize, rhs: usize) -> bool {
        self.find_unchecked(lhs) == self.find_unchecked(rhs)
    }
    #[inline]
    pub fn join(&self, x: usize, y: usize) -> usize {
        let mut a = self.find(x);
        let mut b = self.find(y);
        //SAFETY: self.find(x) guarantees its result is in-bounds
        let a_rank = unsafe { self.rank_unchecked(a) }.get();
        let b_rank = unsafe { self.rank_unchecked(b) }.get();
        if b_rank >= a_rank {
            mem::swap(&mut a, &mut b);
        }
        if a_rank == b_rank {
            unsafe { self.rank_unchecked(b) }.set(a_rank + 1);
        }
        //SAFETY: a and b are bounds-checked by parent_of
        unsafe { self.set_parent_unchecked(a, b) };
        b
    }
    #[inline]
    pub unsafe fn join_unchecked(&self, x: usize, y: usize) -> usize {
        //SAFETY: caller guarantees x and y are in-bounds
        let mut a = self.find_unchecked(x);
        let mut b = self.find_unchecked(y);
        //SAFETY: self.find(x) guarantees its result is in-bounds
        let a_rank = unsafe { self.rank_unchecked(a) }.get();
        let b_rank = unsafe { self.rank_unchecked(b) }.get();
        if b_rank >= a_rank {
            mem::swap(&mut a, &mut b);
        }
        if a_rank == b_rank {
            unsafe { self.rank_unchecked(b) }.set(a_rank + 1);
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
    pub fn new_elem(&mut self) -> usize {
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
