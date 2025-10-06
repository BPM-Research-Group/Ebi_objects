use crate::{
    Activity, parallel_trace_iterator::ParallelTraceIterator, trace_iterator::TraceIterator,
};
use ebi_arithmetic::Fraction;

pub trait IndexTrace: Sync {
    fn number_of_traces(&self) -> usize;

    fn iter_traces(&self) -> TraceIterator<'_>;

    fn par_iter_traces(&self) -> ParallelTraceIterator<'_>;
}

pub trait IndexTraceProbability: Sync {
    fn get_trace_probability(&self, trace_index: usize) -> Option<(&Vec<Activity>, &Fraction)>;

    fn iter_traces_probabilities(
        &'_ self,
    ) -> std::collections::hash_map::Iter<'_, Vec<Activity>, Fraction>;

    fn par_iter_traces_probabilities(
        &self,
    ) -> rayon::collections::hash_map::Iter<'_, Vec<Activity>, Fraction>;
}
