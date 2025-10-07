use crate::Activity;
use ebi_arithmetic::Fraction;

/// Iterator over references to traces.
pub enum RefTraceIterator<'a> {
    Vec(std::slice::Iter<'a, Vec<Activity>>),
    HashSet(std::collections::hash_set::Iter<'a, Vec<Activity>>),
    Keys(std::collections::hash_map::Keys<'a, Vec<Activity>, Fraction>),
}

impl<'a> Iterator for RefTraceIterator<'a> {
    type Item = &'a Vec<Activity>;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            RefTraceIterator::Vec(iter) => iter.next(),
            RefTraceIterator::HashSet(iter) => iter.next(),
            RefTraceIterator::Keys(iter) => iter.next(),
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        match self {
            RefTraceIterator::Vec(iter) => iter.size_hint(),
            RefTraceIterator::HashSet(iter) => iter.size_hint(),
            RefTraceIterator::Keys(iter) => iter.size_hint(),
        }
    }

    fn count(self) -> usize
    where
        Self: Sized,
    {
        match self {
            RefTraceIterator::Vec(iter) => iter.count(),
            RefTraceIterator::HashSet(iter) => iter.count(),
            RefTraceIterator::Keys(iter) => iter.count(),
        }
    }

    fn last(self) -> Option<Self::Item>
    where
        Self: Sized,
    {
        match self {
            RefTraceIterator::Vec(iter) => iter.last(),
            RefTraceIterator::HashSet(iter) => iter.last(),
            RefTraceIterator::Keys(iter) => iter.last(),
        }
    }

    fn nth(&mut self, n: usize) -> Option<Self::Item> {
        match self {
            RefTraceIterator::Vec(iter) => iter.nth(n),
            RefTraceIterator::HashSet(iter) => iter.nth(n),
            RefTraceIterator::Keys(iter) => iter.nth(n),
        }
    }

    fn collect<B: FromIterator<Self::Item>>(self) -> B
    where
        Self: Sized,
    {
        match self {
            RefTraceIterator::Vec(iter) => iter.collect(),
            RefTraceIterator::HashSet(iter) => iter.collect(),
            RefTraceIterator::Keys(iter) => iter.collect(),
        }
    }

    fn position<P>(&mut self, predicate: P) -> Option<usize>
    where
        Self: Sized,
        P: FnMut(Self::Item) -> bool,
    {
        match self {
            RefTraceIterator::Vec(iter) => iter.position(predicate),
            RefTraceIterator::HashSet(iter) => iter.position(predicate),
            RefTraceIterator::Keys(iter) => iter.position(predicate),
        }
    }

    fn sum<S>(self) -> S
    where
        Self: Sized,
        S: std::iter::Sum<Self::Item>,
    {
        match self {
            RefTraceIterator::Vec(iter) => iter.sum(),
            RefTraceIterator::HashSet(iter) => iter.sum(),
            RefTraceIterator::Keys(iter) => iter.sum(),
        }
    }

    fn product<P>(self) -> P
    where
        Self: Sized,
        P: std::iter::Product<Self::Item>,
    {
        match self {
            RefTraceIterator::Vec(iter) => iter.product(),
            RefTraceIterator::HashSet(iter) => iter.product(),
            RefTraceIterator::Keys(iter) => iter.product(),
        }
    }
}
