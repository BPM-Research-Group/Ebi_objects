use ebi_arithmetic::Fraction;
use rayon::iter::{
    IndexedParallelIterator, ParallelIterator,
    plumbing::{Producer, bridge},
};

use crate::{Activity, FiniteStochasticLanguage, NumberOfTraces};

/**
 * A parallel iterator over references to traces.
 *
 * As Rust doesn't allow generic types in traits, this is an enum.
 */
pub struct ParallelRefProbabilityTraceIterator<'a> {
    slang: &'a FiniteStochasticLanguage,
}

impl<'a> From<&'a FiniteStochasticLanguage> for ParallelRefProbabilityTraceIterator<'a> {
    fn from(value: &'a FiniteStochasticLanguage) -> Self {
        Self { slang: value }
    }
}

impl<'a> ParallelIterator for ParallelRefProbabilityTraceIterator<'a> {
    type Item = (&'a Vec<Activity>, &'a Fraction);

    fn drive_unindexed<C>(self, consumer: C) -> C::Result
    where
        C: rayon::iter::plumbing::UnindexedConsumer<Self::Item>,
    {
        bridge(self, consumer)
    }

    fn opt_len(&self) -> Option<usize> {
        Some(self.len())
    }
}

impl<'a> IndexedParallelIterator for ParallelRefProbabilityTraceIterator<'a> {
    fn len(&self) -> usize {
        self.slang.number_of_traces()
    }

    fn drive<C: rayon::iter::plumbing::Consumer<Self::Item>>(self, consumer: C) -> C::Result {
        bridge(self, consumer)
    }

    fn with_producer<CB: rayon::iter::plumbing::ProducerCallback<Self::Item>>(
        self,
        callback: CB,
    ) -> CB::Output {
        let producer = ParallelRefProbabilityTraceIteratorProducer::from(self);
        callback.callback(producer)
    }
}

struct ParallelRefProbabilityTraceIteratorProducer<'a> {
    slang: &'a FiniteStochasticLanguage,
    min: usize,
    max: usize,
}

impl<'a> From<ParallelRefProbabilityTraceIterator<'a>>
    for ParallelRefProbabilityTraceIteratorProducer<'a>
{
    fn from(value: ParallelRefProbabilityTraceIterator<'a>) -> Self {
        let len = value.slang.number_of_traces();
        Self {
            slang: value.slang,
            min: 0,
            max: len,
        }
    }
}

impl<'a> Producer for ParallelRefProbabilityTraceIteratorProducer<'a> {
    type Item = (&'a Vec<Activity>, &'a Fraction);
    type IntoIter = ParallelRefProbabilityTraceIteratorIterator<'a>;

    fn into_iter(self) -> Self::IntoIter {
        ParallelRefProbabilityTraceIteratorIterator::new(self.slang, self.min, self.max)
    }

    fn split_at(self, index: usize) -> (Self, Self) {
        let split = self.min + index;
        (
            Self {
                slang: self.slang,
                min: self.min,
                max: split,
            },
            Self {
                slang: self.slang,
                min: split,
                max: self.max,
            },
        )
    }
}

struct ParallelRefProbabilityTraceIteratorIterator<'a> {
    iter: std::collections::hash_map::Iter<'a, Vec<Activity>, Fraction>,
    left: usize,
}

impl<'a> ParallelRefProbabilityTraceIteratorIterator<'a> {
    fn new(value: &'a FiniteStochasticLanguage, min: usize, max: usize) -> Self {
        let mut iter = value.traces.iter();
        if min > 0 {
            iter.nth(min - 1);
        }
        Self {
            iter,
            left: max - min,
        }
    }
}

impl<'a> Iterator for ParallelRefProbabilityTraceIteratorIterator<'a> {
    type Item = (&'a Vec<Activity>, &'a Fraction);
    fn next(&mut self) -> Option<Self::Item> {
        if self.left == 0 {
            None
        } else {
            let result = self.iter.next();
            self.left -= 1;
            result
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.left, Some(self.left))
    }
}

impl<'a> ExactSizeIterator for ParallelRefProbabilityTraceIteratorIterator<'a> {
    fn len(&self) -> usize {
        let (lower, upper) = self.size_hint();
        // Note: This assertion is overly defensive, but it checks the invariant
        // guaranteed by the trait. If this trait were rust-internal,
        // we could use debug_assert!; assert_eq! will check all Rust user
        // implementations too.
        std::assert_eq!(upper, Some(lower));
        lower
    }
}

impl<'a> DoubleEndedIterator for ParallelRefProbabilityTraceIteratorIterator<'a> {
    fn next_back(&mut self) -> Option<Self::Item> {
        if self.left == 0 {
            None
        } else {
            let mut iter = self.iter.clone();
            let result = iter.nth(self.left);
            self.left -= 1;
            result
        }
    }
}
