use crate::{Activity, EventLogTraceAttributes, FiniteStochasticLanguage, NumberOfTraces};
use ebi_arithmetic::Fraction;
use rayon::iter::{
    IndexedParallelIterator, ParallelIterator,
    plumbing::{Consumer, Producer, ProducerCallback, UnindexedConsumer, bridge},
};

/// A parallel iterator over traces. Note that the traces are created on iteration.
pub struct ParallelTraceIterator<'a> {
    log: &'a EventLogTraceAttributes,
}

impl<'a> ParallelIterator for ParallelTraceIterator<'a> {
    type Item = Vec<Activity>;

    fn drive_unindexed<C>(self, consumer: C) -> C::Result
    where
        C: UnindexedConsumer<Self::Item>,
    {
        bridge(self, consumer)
    }

    fn opt_len(&self) -> Option<usize> {
        Some(self.len())
    }
}

impl<'a> From<&'a EventLogTraceAttributes> for ParallelTraceIterator<'a> {
    fn from(value: &'a EventLogTraceAttributes) -> Self {
        Self { log: value }
    }
}

impl<'a> IndexedParallelIterator for ParallelTraceIterator<'a> {
    fn with_producer<CB: ProducerCallback<Self::Item>>(self, callback: CB) -> CB::Output {
        let producer = ParallelTraceAttributesIteratorDataProducer::from(self);
        callback.callback(producer)
    }

    fn drive<C: Consumer<Self::Item>>(self, consumer: C) -> C::Result {
        bridge(self, consumer)
    }

    fn len(&self) -> usize {
        self.log.number_of_traces()
    }
}

struct ParallelTraceAttributesIteratorDataProducer<'a> {
    log: &'a EventLogTraceAttributes,
    traces: &'a [process_mining::event_log::Trace],
}

impl<'a> From<ParallelTraceIterator<'a>> for ParallelTraceAttributesIteratorDataProducer<'a> {
    fn from(iterator: ParallelTraceIterator<'a>) -> Self {
        Self {
            log: iterator.log,
            traces: &iterator.log.rust4pm_log.traces,
        }
    }
}

impl<'a> Producer for ParallelTraceAttributesIteratorDataProducer<'a> {
    type Item = Vec<Activity>;
    type IntoIter = Rust4PMIterator<'a>;

    fn into_iter(self) -> Self::IntoIter {
        Rust4PMIterator {
            log: self.log,
            traces: self.traces,
        }
    }

    fn split_at(self, index: usize) -> (Self, Self) {
        let (left, right) = self.traces.split_at(index);
        (
            ParallelTraceAttributesIteratorDataProducer {
                log: self.log,
                traces: left,
            },
            ParallelTraceAttributesIteratorDataProducer {
                log: self.log,
                traces: right,
            },
        )
    }
}

struct Rust4PMIterator<'a> {
    log: &'a EventLogTraceAttributes,
    traces: &'a [process_mining::event_log::Trace],
}

impl<'a> Iterator for Rust4PMIterator<'a> {
    type Item = Vec<Activity>;

    fn next(&mut self) -> Option<Self::Item> {
        let trace = self.traces.get(0)?;
        let mut result = Vec::with_capacity(trace.events.len());
        for event in trace.events.iter() {
            let activity = self
                .log
                .activity_key
                .process_activity_attempt(&self.log.classifier.get_class_identity(event))?;
            result.push(activity);
        }

        self.traces = &self.traces[1..];
        Some(result)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.traces.len(), Some(self.traces.len()))
    }
}

impl<'a> DoubleEndedIterator for Rust4PMIterator<'a> {
    fn next_back(&mut self) -> Option<Self::Item> {
        let len = self.traces.len();
        if len > 0 {
            let trace = &self.traces[len - 1];

            let mut result = Vec::with_capacity(trace.events.len());
            for event in trace.events.iter() {
                let activity = self
                    .log
                    .activity_key
                    .process_activity_attempt(&self.log.classifier.get_class_identity(event))?;
                result.push(activity);
            }

            self.traces = &self.traces[0..(len - 1)];
            Some(result)
        } else {
            None
        }
    }
}

impl<'a> ExactSizeIterator for Rust4PMIterator<'a> {
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

pub struct ParallelTraceProbabilitiesIterator<'a> {
    slang: &'a FiniteStochasticLanguage,
}

impl<'a> ParallelIterator for ParallelTraceProbabilitiesIterator<'a> {
    type Item = (&'a Vec<Activity>, &'a Fraction);

    fn drive_unindexed<C>(self, consumer: C) -> C::Result
    where
        C: rayon::iter::plumbing::UnindexedConsumer<Self::Item>,
    {
        bridge(self, consumer)
    }
}

impl<'a> IndexedParallelIterator for ParallelTraceProbabilitiesIterator<'a> {
    fn with_producer<CB: ProducerCallback<Self::Item>>(self, callback: CB) -> CB::Output {
        let producer = ParallelTraceProbabilitiesIteratorProducer::from(self);
        callback.callback(producer)
    }

    fn drive<C: Consumer<Self::Item>>(self, consumer: C) -> C::Result {
        bridge(self, consumer)
    }

    fn len(&self) -> usize {
        self.slang.number_of_traces()
    }
}

struct ParallelTraceProbabilitiesIteratorProducer<'a> {
    slang: &'a FiniteStochasticLanguage,
    min: usize,
    max: usize,
}

impl<'a> From<ParallelTraceProbabilitiesIterator<'a>>
    for ParallelTraceProbabilitiesIteratorProducer<'a>
{
    fn from(value: ParallelTraceProbabilitiesIterator<'a>) -> Self {
        let len = value.slang.number_of_traces();
        Self {
            slang: value.slang,
            min: 0,
            max: len,
        }
    }
}

impl<'a> Producer for ParallelTraceProbabilitiesIteratorProducer<'a> {
    type Item = (&'a Vec<Activity>, &'a Fraction);
    type IntoIter = ParallelTraceProbabilitiesIteratorIterator<'a>;

    fn into_iter(self) -> Self::IntoIter {
        let mut iter = self.slang.traces.iter();
        if self.min > 0 {
            iter.nth(self.min - 1);
        }
        ParallelTraceProbabilitiesIteratorIterator {
            iter: iter,
            left: self.max - self.min,
        }
    }

    fn split_at(self, index: usize) -> (Self, Self) {
        let split = self.min + index;
        (
            ParallelTraceProbabilitiesIteratorProducer {
                slang: self.slang,
                min: self.min,
                max: split,
            },
            ParallelTraceProbabilitiesIteratorProducer {
                slang: self.slang,
                min: split,
                max: self.max,
            },
        )
    }
}

struct ParallelTraceProbabilitiesIteratorIterator<'a> {
    iter: std::collections::hash_map::Iter<'a, Vec<Activity>, Fraction>,
    left: usize,
}

impl<'a> ExactSizeIterator for ParallelTraceProbabilitiesIteratorIterator<'a> {
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

impl<'a> Iterator for ParallelTraceProbabilitiesIteratorIterator<'a> {
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

impl<'a> DoubleEndedIterator for ParallelTraceProbabilitiesIteratorIterator<'a> {
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
