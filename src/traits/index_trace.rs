use crate::Activity;

pub trait IndexTrace: Sync {
    type Iter<'a>: Iterator<Item = &'a Vec<Activity>> where Self: 'a;

    fn number_of_traces(&self) -> usize;

    fn iter<'a>(&'a self) -> Self::Iter<'a>;
}
