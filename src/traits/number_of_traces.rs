use crate::Activity;

pub trait NumberOfTraces {
    fn number_of_traces(&self) -> usize;

    fn number_of_events(&self) -> usize;
}

impl<T> NumberOfTraces for Vec<(Vec<Activity>, T)> {
    fn number_of_traces(&self) -> usize {
        self.len()
    }

    fn number_of_events(&self) -> usize {
        self.iter().map(|(t, _)| t.len()).sum()
    }
}
