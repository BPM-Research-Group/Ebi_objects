use crate::{
    Activity, Attribute, EventLogXes, FiniteStochasticLanguage, HasActivityKey, NumberOfTraces,
    ebi_objects::{event_log_csv::EventLogCsv, event_log_ocel::EventLogOcel},
};
use ebi_arithmetic::Fraction;
use intmap::IntMap;
use rayon::iter::{
    IndexedParallelIterator, ParallelIterator,
    plumbing::{Consumer, Producer, ProducerCallback, UnindexedConsumer, bridge},
};

/// A parallel iterator over traces. Note that the traces are created on iteration.
pub enum ParallelTraceIterator<'a> {
    Csv { log: &'a EventLogCsv },
    Xes { log: &'a EventLogXes },
    Ocel { log: &'a EventLogOcel },
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

impl<'a> IndexedParallelIterator for ParallelTraceIterator<'a> {
    fn len(&self) -> usize {
        match self {
            ParallelTraceIterator::Csv { log } => log.number_of_traces(),
            ParallelTraceIterator::Xes { log } => log.number_of_traces(),
            ParallelTraceIterator::Ocel { log } => log.number_of_traces(),
        }
    }

    fn drive<C: Consumer<Self::Item>>(self, consumer: C) -> C::Result {
        bridge(self, consumer)
    }

    fn with_producer<CB: ProducerCallback<Self::Item>>(self, callback: CB) -> CB::Output {
        match self {
            ParallelTraceIterator::Csv { log } => {
                let producer = ParallelTraceIteratorDataProducerCsv::from(log);
                callback.callback(producer)
            }
            ParallelTraceIterator::Xes { log } => {
                let producer = ParallelTraceIteratorDataProducerXes::from(log);
                callback.callback(producer)
            }
            ParallelTraceIterator::Ocel { log } => {
                let producer = ParallelTraceIteratorDataProducerOcel::from(log);
                callback.callback(producer)
            }
        }
    }
}

impl<'a> From<&'a EventLogCsv> for ParallelTraceIterator<'a> {
    fn from(value: &'a EventLogCsv) -> Self {
        Self::Csv { log: value }
    }
}

impl<'a> From<&'a EventLogXes> for ParallelTraceIterator<'a> {
    fn from(value: &'a EventLogXes) -> Self {
        Self::Xes { log: value }
    }
}

impl<'a> From<&'a EventLogOcel> for ParallelTraceIterator<'a> {
    fn from(value: &'a EventLogOcel) -> Self {
        Self::Ocel { log: value }
    }
}

// === CSV ===

struct ParallelTraceIteratorDataProducerCsv<'a> {
    log: &'a EventLogCsv,
    traces: &'a [(String, Vec<IntMap<Attribute, String>>)],
}

impl<'a> From<&'a EventLogCsv> for ParallelTraceIteratorDataProducerCsv<'a> {
    fn from(value: &'a EventLogCsv) -> Self {
        Self {
            log: value,
            traces: &value.traces,
        }
    }
}

impl<'a> Producer for ParallelTraceIteratorDataProducerCsv<'a> {
    type Item = Vec<Activity>;
    type IntoIter = ParallelIteratorCsv<'a>;

    fn into_iter(self) -> Self::IntoIter {
        ParallelIteratorCsv {
            log: self.log,
            traces: self.traces,
        }
    }

    fn split_at(self, index: usize) -> (Self, Self) {
        let (left, right) = self.traces.split_at(index);
        (
            ParallelTraceIteratorDataProducerCsv {
                log: self.log,
                traces: left,
            },
            ParallelTraceIteratorDataProducerCsv {
                log: self.log,
                traces: right,
            },
        )
    }
}

struct ParallelIteratorCsv<'a> {
    log: &'a EventLogCsv,
    traces: &'a [(String, Vec<IntMap<Attribute, String>>)],
}

impl<'a> Iterator for ParallelIteratorCsv<'a> {
    type Item = Vec<Activity>;

    fn next(&mut self) -> Option<Self::Item> {
        let (_, trace) = self.traces.get(0)?;
        let mut result = Vec::with_capacity(trace.len());
        for event in trace.iter() {
            let empty = String::new();
            let activity_label = event
                .get(self.log.activity_attribute)
                .unwrap_or_else(|| &empty);
            let activity = self
                .log
                .activity_key
                .process_activity_attempt(&activity_label)?;
            result.push(activity);
        }

        self.traces = &self.traces[1..];
        Some(result)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.traces.len(), Some(self.traces.len()))
    }
}

impl<'a> DoubleEndedIterator for ParallelIteratorCsv<'a> {
    fn next_back(&mut self) -> Option<Self::Item> {
        let len = self.traces.len();
        if len > 0 {
            let (_, trace) = &self.traces[len - 1];

            let mut result = Vec::with_capacity(trace.len());
            for event in trace.iter() {
                let empty = String::new();
                let activity_label = event
                    .get(self.log.activity_attribute)
                    .unwrap_or_else(|| &empty);
                let activity = self
                    .log
                    .activity_key
                    .process_activity_attempt(&activity_label)?;
                result.push(activity);
            }

            self.traces = &self.traces[0..(len - 1)];
            Some(result)
        } else {
            None
        }
    }
}

impl<'a> ExactSizeIterator for ParallelIteratorCsv<'a> {
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

// === XES ===

struct ParallelTraceIteratorDataProducerXes<'a> {
    log: &'a EventLogXes,
    traces: &'a [process_mining::core::event_data::case_centric::Trace],
}

impl<'a> From<&'a EventLogXes> for ParallelTraceIteratorDataProducerXes<'a> {
    fn from(value: &'a EventLogXes) -> Self {
        Self {
            log: value,
            traces: &value.rust4pm_log.traces,
        }
    }
}

impl<'a> Producer for ParallelTraceIteratorDataProducerXes<'a> {
    type Item = Vec<Activity>;
    type IntoIter = ParallelIteratorXes<'a>;

    fn into_iter(self) -> Self::IntoIter {
        ParallelIteratorXes {
            log: self.log,
            traces: self.traces,
        }
    }

    fn split_at(self, index: usize) -> (Self, Self) {
        let (left, right) = self.traces.split_at(index);
        (
            ParallelTraceIteratorDataProducerXes {
                log: self.log,
                traces: left,
            },
            ParallelTraceIteratorDataProducerXes {
                log: self.log,
                traces: right,
            },
        )
    }
}

struct ParallelIteratorXes<'a> {
    log: &'a EventLogXes,
    traces: &'a [process_mining::core::event_data::case_centric::Trace],
}

impl<'a> Iterator for ParallelIteratorXes<'a> {
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

impl<'a> DoubleEndedIterator for ParallelIteratorXes<'a> {
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

impl<'a> ExactSizeIterator for ParallelIteratorXes<'a> {
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

// === OCEL ===

struct ParallelTraceIteratorDataProducerOcel<'a> {
    log: &'a EventLogOcel,
    objects: Vec<String>,
}

impl<'a> From<&'a EventLogOcel> for ParallelTraceIteratorDataProducerOcel<'a> {
    fn from(value: &'a EventLogOcel) -> Self {
        Self {
            log: value,
            objects: EventLogOcel::get_relevant_objects(
                &value.rust4pm_log.objects,
                &value.object_type,
            )
            .into_iter()
            .collect(),
        }
    }
}

impl<'a> Producer for ParallelTraceIteratorDataProducerOcel<'a> {
    type Item = Vec<Activity>;
    type IntoIter = ParallelIteratorOcel<'a>;

    fn into_iter(self) -> Self::IntoIter {
        ParallelIteratorOcel {
            log: self.log,
            objects: self.objects,
        }
    }

    fn split_at(self, index: usize) -> (Self, Self) {
        let (left, right) = self.objects.split_at(index);
        (
            ParallelTraceIteratorDataProducerOcel {
                log: self.log,
                objects: left.to_owned(),
            },
            ParallelTraceIteratorDataProducerOcel {
                log: self.log,
                objects: right.to_owned(),
            },
        )
    }
}

struct ParallelIteratorOcel<'a> {
    log: &'a EventLogOcel,
    objects: Vec<String>,
}

impl<'a> Iterator for ParallelIteratorOcel<'a> {
    type Item = Vec<Activity>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(object_id) = self.objects.pop() {
            //gather the relevant events
            Some(
                self.log
                    .rust4pm_log
                    .events
                    .iter()
                    .filter(|event| {
                        event
                            .relationships
                            .iter()
                            .any(|relation| relation.object_id == object_id)
                    })
                    .filter_map(|event| {
                        self.log
                            .activity_key()
                            .process_activity_attempt(&event.event_type)
                    })
                    .collect(),
            )
        } else {
            None
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.objects.len(), Some(self.objects.len()))
    }
}

impl<'a> DoubleEndedIterator for ParallelIteratorOcel<'a> {
    fn next_back(&mut self) -> Option<Self::Item> {
        if self.objects.len() > 0 {
            let object_id = self.objects.remove(0);
            //gather the relevant events
            Some(
                self.log
                    .rust4pm_log
                    .events
                    .iter()
                    .filter(|event| {
                        event
                            .relationships
                            .iter()
                            .any(|relation| relation.object_id == object_id)
                    })
                    .filter_map(|event| {
                        self.log
                            .activity_key()
                            .process_activity_attempt(&event.event_type)
                    })
                    .collect(),
            )
        } else {
            None
        }
    }
}

impl<'a> ExactSizeIterator for ParallelIteratorOcel<'a> {
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

// === other ===

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
