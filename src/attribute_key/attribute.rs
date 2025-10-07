use std::{
    fmt::{Debug, Display},
    hash::Hash,
};

#[derive(Clone, Copy)]
pub struct Attribute {
    pub(crate) id: usize,
}

impl PartialEq for Attribute {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl PartialEq<usize> for Attribute {
    fn eq(&self, other: &usize) -> bool {
        &self.id == other
    }
}

impl Eq for Attribute {}

impl Hash for Attribute {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

impl Display for Attribute {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "at{}", self.id)
    }
}

impl Debug for Attribute {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "at{}", self.id)
    }
}

impl PartialOrd for Attribute {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.id.partial_cmp(&other.id)
    }
}

impl PartialOrd<usize> for Attribute {
    fn partial_cmp(&self, other: &usize) -> Option<std::cmp::Ordering> {
        self.id.partial_cmp(other)
    }
}
