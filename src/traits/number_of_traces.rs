use crate::Activity;

pub trait NumberOfTraces {
    fn number_of_traces(&self) -> usize;
}

impl<T> NumberOfTraces for Vec<(Vec<Activity>, T)> {
    fn number_of_traces(&self) -> usize {
        self.len()
    }
}
