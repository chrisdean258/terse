use crate::types::DeclarationKind;
use std::collections::HashMap;

#[derive(Debug)]
pub struct ScopeTable<T> {
    scopes: Vec<HashMap<String, (T, DeclarationKind)>>,
}

pub enum GetMutError {
    NoSuchKey,
    IsLetVar,
}

impl<T> ScopeTable<T> {
    pub fn new(presets: HashMap<String, (T, DeclarationKind)>) -> Self {
        Self {
            scopes: vec![presets],
        }
    }
    pub fn insert(&mut self, key: String, val: T, kind: DeclarationKind) {
        self.scopes
            .last_mut()
            .expect("scopes is empty")
            .insert(key, (val, kind));
    }

    pub fn get(&mut self, key: &str) -> Option<&mut T> {
        for scope in self.scopes.iter_mut().rev() {
            if let Some((v, _)) = scope.get_mut(key) {
                return Some(v);
            }
        }
        None
    }

    pub fn get_mut(&mut self, key: &str) -> Result<&mut T, GetMutError> {
        for scope in self.scopes.iter_mut().rev() {
            if let Some((v, k)) = scope.get_mut(key) {
                return match k {
                    DeclarationKind::Var => Ok(v),
                    DeclarationKind::Let => Err(GetMutError::IsLetVar),
                };
            }
        }
        Err(GetMutError::NoSuchKey)
    }

    pub fn open(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn close(&mut self) {
        self.scopes.pop();
        assert!(!self.scopes.is_empty(), "popped last scope");
    }
}

impl ScopeTable<crate::value::Value> {
    pub fn clone_or_take(&mut self, key: &str) -> Option<crate::value::Value> {
        for scope in self.scopes.iter_mut().rev() {
            if let Some((v, _)) = scope.get_mut(key) {
                // we can take out of `let` or `var` decls
                return Some(v.clone_or_take());
            }
        }
        None
    }
}
