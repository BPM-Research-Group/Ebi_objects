#[cfg(test)]
use uuid::Uuid;

use std::{
    fmt::{Debug, Display},
    hash::Hash,
};

#[derive(Clone, Copy, Ord, Eq, PartialEq, PartialOrd)]
#[cfg(not(test))]
pub struct Activity {
    pub id: usize,
}

#[derive(Clone, Copy, Eq)]
#[cfg(test)]
pub struct Activity {
    pub id: usize,
    pub activity_key_uuid: Uuid, //In testing, an uuid is kept of the activity key.
}

impl PartialEq<usize> for Activity {
    fn eq(&self, other: &usize) -> bool {
        &self.id == other
    }
}

impl Hash for Activity {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

impl Display for Activity {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "ac{}", self.id)
    }
}

impl Debug for Activity {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "ac{}", self.id)
    }
}

impl PartialOrd<usize> for Activity {
    fn partial_cmp(&self, other: &usize) -> Option<std::cmp::Ordering> {
        self.id.partial_cmp(other)
    }
}

#[cfg(test)]
impl PartialEq for Activity {
    fn eq(&self, other: &Self) -> bool {
        assert!(
            self.activity_key_uuid == other.activity_key_uuid,
            "cannot compare activities of different activity keys"
        );

        self.id == other.id
    }
}

#[cfg(test)]
impl Ord for Activity {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        assert!(
            self.activity_key_uuid == other.activity_key_uuid,
            "cannot compare activities of different activity keys"
        );

        self.id.cmp(&other.id)
    }
}

#[cfg(test)]
impl PartialOrd for Activity {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        assert!(
            self.activity_key_uuid == other.activity_key_uuid,
            "cannot compare activities of different activity keys"
        );

        self.id.partial_cmp(&other.id)
    }
}
