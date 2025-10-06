use ebi_arithmetic::Fraction;

use crate::Activity;

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

    fn for_each<F>(self, f: F)
    where
        Self: Sized,
        F: FnMut(Self::Item),
    {
        match self {
            TraceIterator::Vec(iter) => iter.for_each(f),
            TraceIterator::HashSet(iter) => iter.for_each(f),
            TraceIterator::Keys(iter) => iter.for_each(f),
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

    fn partition<B, F>(self, f: F) -> (B, B)
    where
        Self: Sized,
        B: Default + Extend<Self::Item>,
        F: FnMut(&Self::Item) -> bool,
    {
        match self {
            TraceIterator::Vec(iter) => iter.partition(f),
            TraceIterator::HashSet(iter) => iter.partition(f),
            TraceIterator::Keys(iter) => iter.partition(f),
        }
    }

    fn fold<B, F>(self, init: B, f: F) -> B
    where
        Self: Sized,
        F: FnMut(B, Self::Item) -> B,
    {
        match self {
            TraceIterator::Vec(iter) => iter.fold(init, f),
            TraceIterator::HashSet(iter) => iter.fold(init, f),
            TraceIterator::Keys(iter) => iter.fold(init, f),
        }
    }

    fn reduce<F>(self, f: F) -> Option<Self::Item>
    where
        Self: Sized,
        F: FnMut(Self::Item, Self::Item) -> Self::Item,
    {
        match self {
            TraceIterator::Vec(iter) => iter.reduce(f),
            TraceIterator::HashSet(iter) => iter.reduce(f),
            TraceIterator::Keys(iter) => iter.reduce(f),
        }
    }

    fn all<F>(&mut self, f: F) -> bool
    where
        Self: Sized,
        F: FnMut(Self::Item) -> bool,
    {
        match self {
            TraceIterator::Vec(iter) => iter.all(f),
            TraceIterator::HashSet(iter) => iter.all(f),
            TraceIterator::Keys(iter) => iter.all(f),
        }
    }

    fn any<F>(&mut self, f: F) -> bool
    where
        Self: Sized,
        F: FnMut(Self::Item) -> bool,
    {
        match self {
            TraceIterator::Vec(iter) => iter.any(f),
            TraceIterator::HashSet(iter) => iter.any(f),
            TraceIterator::Keys(iter) => iter.any(f),
        }
    }

    fn find<P>(&mut self, predicate: P) -> Option<Self::Item>
    where
        Self: Sized,
        P: FnMut(&Self::Item) -> bool,
    {
        match self {
            TraceIterator::Vec(iter) => iter.find(predicate),
            TraceIterator::HashSet(iter) => iter.find(predicate),
            TraceIterator::Keys(iter) => iter.find(predicate),
        }
    }

    fn find_map<B, F>(&mut self, f: F) -> Option<B>
    where
        Self: Sized,
        F: FnMut(Self::Item) -> Option<B>,
    {
        match self {
            TraceIterator::Vec(iter) => iter.find_map(f),
            TraceIterator::HashSet(iter) => iter.find_map(f),
            TraceIterator::Keys(iter) => iter.find_map(f),
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

    fn max(self) -> Option<Self::Item>
    where
        Self: Sized,
        Self::Item: Ord,
    {
        match self {
            TraceIterator::Vec(iter) => iter.max(),
            TraceIterator::HashSet(iter) => iter.max(),
            TraceIterator::Keys(iter) => iter.max(),
        }
    }

    fn min(self) -> Option<Self::Item>
    where
        Self: Sized,
        Self::Item: Ord,
    {
        match self {
            TraceIterator::Vec(iter) => iter.min(),
            TraceIterator::HashSet(iter) => iter.min(),
            TraceIterator::Keys(iter) => iter.min(),
        }
    }

    fn max_by_key<B: Ord, F>(self, f: F) -> Option<Self::Item>
    where
        Self: Sized,
        F: FnMut(&Self::Item) -> B,
    {
        match self {
            TraceIterator::Vec(iter) => iter.max_by_key(f),
            TraceIterator::HashSet(iter) => iter.max_by_key(f),
            TraceIterator::Keys(iter) => iter.max_by_key(f),
        }
    }

    fn max_by<F>(self, compare: F) -> Option<Self::Item>
    where
        Self: Sized,
        F: FnMut(&Self::Item, &Self::Item) -> std::cmp::Ordering,
    {
        match self {
            TraceIterator::Vec(iter) => iter.max_by(compare),
            TraceIterator::HashSet(iter) => iter.max_by(compare),
            TraceIterator::Keys(iter) => iter.max_by(compare),
        }
    }

    fn min_by_key<B: Ord, F>(self, f: F) -> Option<Self::Item>
    where
        Self: Sized,
        F: FnMut(&Self::Item) -> B,
    {
        match self {
            TraceIterator::Vec(iter) => iter.min_by_key(f),
            TraceIterator::HashSet(iter) => iter.min_by_key(f),
            TraceIterator::Keys(iter) => iter.min_by_key(f),
        }
    }

    fn min_by<F>(self, compare: F) -> Option<Self::Item>
    where
        Self: Sized,
        F: FnMut(&Self::Item, &Self::Item) -> std::cmp::Ordering,
    {
        match self {
            TraceIterator::Vec(iter) => iter.min_by(compare),
            TraceIterator::HashSet(iter) => iter.min_by(compare),
            TraceIterator::Keys(iter) => iter.min_by(compare),
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

    fn cmp<I>(self, other: I) -> std::cmp::Ordering
    where
        I: IntoIterator<Item = Self::Item>,
        Self::Item: Ord,
        Self: Sized,
    {
        match self {
            TraceIterator::Vec(iter) => iter.cmp(other),
            TraceIterator::HashSet(iter) => iter.cmp(other),
            TraceIterator::Keys(iter) => iter.cmp(other),
        }
    }

    fn partial_cmp<I>(self, other: I) -> Option<std::cmp::Ordering>
    where
        I: IntoIterator,
        Self::Item: PartialOrd<I::Item>,
        Self: Sized,
    {
        match self {
            TraceIterator::Vec(iter) => iter.partial_cmp(other),
            TraceIterator::HashSet(iter) => iter.partial_cmp(other),
            TraceIterator::Keys(iter) => iter.partial_cmp(other),
        }
    }
}
