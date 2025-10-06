use ebi_arithmetic::Fraction;

use crate::{Activity, EventLogTraceAttributes};

pub enum TraceIterator<'a> {
    Vec(std::slice::Iter<'a, Vec<Activity>>),
    HashSet(std::collections::hash_set::Iter<'a, Vec<Activity>>),
    Keys(std::collections::hash_map::Keys<'a, Vec<Activity>, Fraction>),
}

impl<'a> Iterator for TraceIterator<'a> {
    type Item = &'a Vec<Activity>;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            TraceIterator::Vec(iter) => iter.next(),
            TraceIterator::HashSet(iter) => iter.next(),
            TraceIterator::Keys(iter) => iter.next(),
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        match self {
            TraceIterator::Vec(iter) => iter.size_hint(),
            TraceIterator::HashSet(iter) => iter.size_hint(),
            TraceIterator::Keys(iter) => iter.size_hint(),
        }
    }

    fn count(self) -> usize
    where
        Self: Sized,
    {
        match self {
            TraceIterator::Vec(iter) => iter.count(),
            TraceIterator::HashSet(iter) => iter.count(),
            TraceIterator::Keys(iter) => iter.count(),
        }
    }

    fn last(self) -> Option<Self::Item>
    where
        Self: Sized,
    {
        match self {
            TraceIterator::Vec(iter) => iter.last(),
            TraceIterator::HashSet(iter) => iter.last(),
            TraceIterator::Keys(iter) => iter.last(),
        }
    }

    fn nth(&mut self, n: usize) -> Option<Self::Item> {
        match self {
            TraceIterator::Vec(iter) => iter.nth(n),
            TraceIterator::HashSet(iter) => iter.nth(n),
            TraceIterator::Keys(iter) => iter.nth(n),
        }
    }

    fn collect<B: FromIterator<Self::Item>>(self) -> B
    where
        Self: Sized,
    {
        match self {
            TraceIterator::Vec(iter) => iter.collect(),
            TraceIterator::HashSet(iter) => iter.collect(),
            TraceIterator::Keys(iter) => iter.collect(),
        }
    }

    fn position<P>(&mut self, predicate: P) -> Option<usize>
    where
        Self: Sized,
        P: FnMut(Self::Item) -> bool,
    {
        match self {
            TraceIterator::Vec(iter) => iter.position(predicate),
            TraceIterator::HashSet(iter) => iter.position(predicate),
            TraceIterator::Keys(iter) => iter.position(predicate),
        }
    }

    fn sum<S>(self) -> S
    where
        Self: Sized,
        S: std::iter::Sum<Self::Item>,
    {
        match self {
            TraceIterator::Vec(iter) => iter.sum(),
            TraceIterator::HashSet(iter) => iter.sum(),
            TraceIterator::Keys(iter) => iter.sum(),
        }
    }

    fn product<P>(self) -> P
    where
        Self: Sized,
        P: std::iter::Product<Self::Item>,
    {
        match self {
            TraceIterator::Vec(iter) => iter.product(),
            TraceIterator::HashSet(iter) => iter.product(),
            TraceIterator::Keys(iter) => iter.product(),
        }
    }
}

pub struct EventLogTraceAttributesIterator<'a> {
    log: &'a EventLogTraceAttributes,
    next: usize,
}

impl<'a> Iterator for EventLogTraceAttributesIterator<'a> {
    type Item = Vec<Activity>;

    fn next(&mut self) -> Option<Self::Item> {
        let trace = self.log.rust4pm_log.traces.get(self.next)?;
        let mut result = Vec::with_capacity(trace.events.len());
        for event in trace.events.iter() {
            let activity = self
                .log
                .activity_key
                .process_activity_attempt(&self.log.classifier.get_class_identity(event))?;
            result.push(activity);
        }
        self.next += 1;
        Some(result)
    }
}

impl<'a> From<&'a EventLogTraceAttributes> for EventLogTraceAttributesIterator<'a> {
    fn from(value: &'a EventLogTraceAttributes) -> Self {
        Self {
            log: value,
            next: 0,
        }
    }
}
