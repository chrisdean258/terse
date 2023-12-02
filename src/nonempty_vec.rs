#![allow(dead_code)]
use std::ops::{Deref, DerefMut};
pub struct NonEmptyVec<T> {
    internal: Vec<T>,
}

impl<T> NonEmptyVec<T> {
    pub fn new(item: T) -> Self {
        Self {
            internal: vec![item],
        }
    }

    pub fn pop(mut self) -> Result<(Self, T), T> {
        if self.internal.len() == 1 {
            Err(self.internal.pop().unwrap())
        } else {
            let val = self.internal.pop().unwrap();
            Ok((self, val))
        }
    }

    pub fn first(&self) -> &T {
        self.internal.first().unwrap()
    }

    pub fn last(&self) -> &T {
        self.internal.first().unwrap()
    }

    pub fn first_mut(&mut self) -> &mut T {
        self.internal.first_mut().unwrap()
    }

    pub fn last_mut(&mut self) -> &mut T {
        self.internal.last_mut().unwrap()
    }
}

impl<T> Deref for NonEmptyVec<T> {
    type Target = Vec<T>;
    fn deref(&self) -> &Self::Target {
        &self.internal
    }
}

impl<T> DerefMut for NonEmptyVec<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.internal
    }
}
