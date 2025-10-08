use std::collections::HashMap;

use crate::{
    Activity,
    iterators::{
        parallel_ref_probability_trace_iterator::ParallelRefProbabilityTraceIterator,
        parallel_ref_trace_iterator::ParallelRefTraceIterator,
        parallel_trace_iterator::ParallelTraceIterator, ref_trace_iterator::RefTraceIterator,
        trace_iterator::TraceIterator,
    },
};
use ebi_arithmetic::Fraction;

pub trait IntoRefTraceIterator: Sync {
    fn iter_traces(&self) -> RefTraceIterator<'_>;

    fn par_iter_traces(&self) -> ParallelRefTraceIterator<'_>;
}

pub trait IntoTraceIterator {
    fn iter_traces(&'_ self) -> TraceIterator<'_>;

    fn par_iter_traces(&self) -> ParallelTraceIterator<'_>;
}

pub trait IntoRefProbabilityIterator {
    fn iter_probabilities(&self)
    -> std::collections::hash_map::Values<'_, Vec<Activity>, Fraction>;
}

pub trait IntoTraceProbabilityIterator {
    fn into_iter_trace_probabilities(
        self,
    ) -> std::collections::hash_map::IntoIter<Vec<Activity>, Fraction>;

    fn into_par_iter_trace_probabilities(
        self,
    ) -> rayon::collections::hash_map::IntoIter<Vec<Activity>, Fraction>;
}

pub trait IntoRefTraceProbabilityIterator {
    fn iter_traces_probabilities(
        &'_ self,
    ) -> std::collections::hash_map::Iter<'_, Vec<Activity>, Fraction>;

    fn par_iter_traces_probabilities(&'_ self) -> ParallelRefProbabilityTraceIterator<'_>;
}

impl IntoRefTraceIterator for Vec<(Vec<Activity>, HashMap<String, u64>)> {
    fn iter_traces(&self) -> RefTraceIterator<'_> {
        RefTraceIterator::VecTuple(self.into())
    }

    fn par_iter_traces(&self) -> ParallelRefTraceIterator<'_> {
        ParallelRefTraceIterator::VecTuple(self.into())
    }
}
