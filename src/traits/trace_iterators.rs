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

#[cfg(test)]
mod tests {
    use rayon::iter::{IntoParallelIterator, IntoParallelRefIterator, ParallelIterator};
    use std::fs;

    use crate::{
        EventLog, EventLogTraceAttributes, FiniteLanguage, FiniteStochasticLanguage,
        IntoAttributeIterator, IntoAttributeTraceIterator, IntoRefProbabilityIterator,
        IntoRefTraceIterator, IntoRefTraceProbabilityIterator, IntoTraceIterator,
        IntoTraceProbabilityIterator, NumberOfTraces,
        attribute_key::has_attribute_key::HasAttributeKey,
    };

    #[test]
    fn iters_empty() {
        let fin = fs::read_to_string("testfiles/empty.slang").unwrap();
        let slang = fin.parse::<FiniteStochasticLanguage>().unwrap();

        assert_eq!(slang.number_of_traces(), 0);

        assert_eq!(None, slang.iter_probabilities().next());
        assert_eq!(None, slang.iter_traces().next());
        assert_eq!(None, slang.iter_traces_probabilities().next());
        assert_eq!(false, slang.par_iter().any(|_| true));
        assert_eq!(false, slang.par_iter_traces().any(|_| true));
        assert_eq!(false, slang.par_iter_traces_probabilities().any(|_| true));
        assert_eq!(false, slang.clone().into_iter().any(|_| true));
        assert_eq!(false, slang.clone().into_par_iter().any(|_| true));
        assert_eq!(
            false,
            slang.clone().into_iter_trace_probabilities().any(|_| true)
        );
        assert_eq!(
            false,
            slang
                .clone()
                .into_par_iter_trace_probabilities()
                .any(|_| true)
        );

        let fin = fs::read_to_string("testfiles/empty.xes").unwrap();
        let slang = fin.parse::<EventLog>().unwrap();

        assert_eq!(None, slang.iter_traces().next());
        assert_eq!(false, slang.par_iter().any(|_| true));
        assert_eq!(false, slang.par_iter_traces().any(|_| true));
        assert_eq!(false, slang.clone().into_iter().any(|_| true));
        assert_eq!(false, slang.clone().into_par_iter().any(|_| true));

        let fin = fs::read_to_string("testfiles/empty.lang").unwrap();
        let slang = fin.parse::<FiniteLanguage>().unwrap();

        assert_eq!(None, slang.iter_traces().next());
        assert_eq!(false, slang.par_iter().any(|_| true));
        assert_eq!(false, slang.par_iter_traces().any(|_| true));
        assert_eq!(false, slang.clone().into_iter().any(|_| true));
        assert_eq!(false, slang.clone().into_par_iter().any(|_| true));

        let fin = fs::read_to_string("testfiles/empty.xes").unwrap();
        let mut slang = fin.parse::<EventLogTraceAttributes>().unwrap();
        let attribute = slang.attribute_key_mut().add_categorical_attribute("test");

        assert_eq!(None, slang.iter_traces().next());
        assert_eq!(false, slang.par_iter_traces().any(|_| true));
        assert_eq!(false, slang.iter_categorical(attribute).any(|_| true));
        assert_eq!(
            false,
            slang.iter_categorical_and_traces(attribute).any(|_| true)
        );
        assert_eq!(false, slang.iter_time(attribute).any(|_| true));
        assert_eq!(false, slang.iter_time_and_traces(attribute).any(|_| true));
        assert_eq!(false, slang.iter_numeric(attribute).any(|_| true));
        assert_eq!(
            false,
            slang.iter_numeric_and_traces(attribute).any(|_| true)
        );
    }

    macro_rules! collect {
        ($t: expr, $u: expr) => {
            assert_eq!($t.collect::<Vec<_>>(), $u.collect::<Vec<_>>());
        };
    }

    #[test]
    fn collect_iters() {
        let fin = fs::read_to_string("testfiles/a-b.slang").unwrap();
        let slang = fin.parse::<FiniteStochasticLanguage>().unwrap();
        collect!(slang.iter_traces(), slang.par_iter_traces());
        collect!(slang.clone().into_iter(), slang.clone().into_par_iter());
        collect!(
            slang.iter_traces_probabilities(),
            slang.par_iter_traces_probabilities()
        );
        collect!(
            slang.clone().into_iter_trace_probabilities(),
            slang.clone().into_par_iter_trace_probabilities()
        );

        let fin = fs::read_to_string("testfiles/a-b.slang").unwrap();
        let slang: FiniteLanguage = fin.parse::<FiniteStochasticLanguage>().unwrap().into();
        collect!(slang.iter_traces(), slang.par_iter_traces());
        collect!(slang.clone().into_iter(), slang.clone().into_par_iter());
    }

    macro_rules! test_log {
        ($l: expr) => {
            let fin = fs::read_to_string($l).unwrap();
            let slang = fin.parse::<EventLog>().unwrap();
            collect!(slang.iter_traces(), slang.par_iter_traces());
            collect!(slang.clone().into_iter(), slang.clone().into_par_iter());

            let fin = fs::read_to_string($l).unwrap();
            let slang = fin.parse::<EventLogTraceAttributes>().unwrap();
            collect!(slang.iter_traces(), slang.par_iter_traces());
        };
    }

    #[test]
    fn test_logs_iters() {
        test_log!("testfiles/a-b.xes");
        test_log!("testfiles/publishing.xes");
    }
}
