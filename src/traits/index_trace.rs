use crate::Activity;

pub trait IndexTrace: Sync {
    fn number_of_traces(&self) -> usize;

    fn iter(&self) -> impl Iterator<Item = &Vec<Activity>>;
}
