use std::collections::HashMap;

use crate::Activity;

pub trait IndexTrace: Sync {
    fn number_of_traces(&self) -> usize;
    fn get_trace(&self, trace_index: usize) -> Option<&Vec<Activity>>;
}

impl<T: Sync> IndexTrace for HashMap<Vec<Activity>, T> {
    fn number_of_traces(&self) -> usize {
        self.len()
    }

    fn get_trace(&self, trace_index: usize) -> Option<&Vec<Activity>> {
        Some(self.iter().nth(trace_index)?.0)
    }
}

impl<T: Sync> IndexTrace for Vec<(&Vec<Activity>, T)> {
    fn number_of_traces(&self) -> usize {
        self.len()
    }

    fn get_trace(&self, trace_index: usize) -> Option<&Vec<Activity>> {
        Some(self.get(trace_index)?.0)
    }
}
