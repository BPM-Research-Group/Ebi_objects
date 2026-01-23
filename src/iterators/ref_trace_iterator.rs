use crate::{Activity, Attribute};
use ebi_arithmetic::Fraction;
use intmap::IntMap;
use process_mining::core::event_data::case_centric::AttributeValue;
use std::collections::HashMap;

/// Iterator over references to traces.
pub enum RefTraceIterator<'a> {
    Vec(std::slice::Iter<'a, Vec<Activity>>),
    VecTupleHashMap(VecTupleIterator<'a, Vec<Activity>, HashMap<String, u64>>),
    VecTupleIntMap(VecTupleIterator<'a, Vec<Activity>, IntMap<Attribute, AttributeValue>>),
    HashSet(std::collections::hash_set::Iter<'a, Vec<Activity>>),
    Keys(std::collections::hash_map::Keys<'a, Vec<Activity>, Fraction>),
}

impl<'a> Iterator for RefTraceIterator<'a> {
    type Item = &'a Vec<Activity>;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            RefTraceIterator::Vec(iter) => iter.next(),
            RefTraceIterator::VecTupleHashMap(iter) => iter.next(),
            RefTraceIterator::VecTupleIntMap(iter) => iter.next(),
            RefTraceIterator::HashSet(iter) => iter.next(),
            RefTraceIterator::Keys(iter) => iter.next(),
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        match self {
            RefTraceIterator::Vec(iter) => iter.size_hint(),
            RefTraceIterator::VecTupleHashMap(iter) => iter.size_hint(),
            RefTraceIterator::VecTupleIntMap(iter) => iter.size_hint(),
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
            RefTraceIterator::VecTupleHashMap(iter) => iter.count(),
            RefTraceIterator::VecTupleIntMap(iter) => iter.count(),
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
            RefTraceIterator::VecTupleHashMap(iter) => iter.last(),
            RefTraceIterator::VecTupleIntMap(iter) => iter.last(),
            RefTraceIterator::HashSet(iter) => iter.last(),
            RefTraceIterator::Keys(iter) => iter.last(),
        }
    }

    fn nth(&mut self, n: usize) -> Option<Self::Item> {
        match self {
            RefTraceIterator::Vec(iter) => iter.nth(n),
            RefTraceIterator::VecTupleHashMap(iter) => iter.nth(n),
            RefTraceIterator::VecTupleIntMap(iter) => iter.nth(n),
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
            RefTraceIterator::VecTupleHashMap(iter) => iter.collect(),
            RefTraceIterator::VecTupleIntMap(iter) => iter.collect(),
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
            RefTraceIterator::VecTupleHashMap(iter) => iter.position(predicate),
            RefTraceIterator::VecTupleIntMap(iter) => iter.position(predicate),
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
            RefTraceIterator::VecTupleHashMap(iter) => iter.sum(),
            RefTraceIterator::VecTupleIntMap(iter) => iter.sum(),
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
            RefTraceIterator::VecTupleHashMap(iter) => iter.product(),
            RefTraceIterator::VecTupleIntMap(iter) => iter.product(),
            RefTraceIterator::HashSet(iter) => iter.product(),
            RefTraceIterator::Keys(iter) => iter.product(),
        }
    }
}

pub struct VecTupleIterator<'a, K, V>(std::slice::Iter<'a, (K, V)>);

impl<'a> From<&'a Vec<(Vec<Activity>, HashMap<String, u64>)>>
    for VecTupleIterator<'a, Vec<Activity>, HashMap<String, u64>>
{
    fn from(value: &'a Vec<(Vec<Activity>, HashMap<String, u64>)>) -> Self {
        Self(value.iter())
    }
}

impl<'a> From<&'a Vec<(Vec<Activity>, IntMap<Attribute, AttributeValue>)>>
    for VecTupleIterator<'a, Vec<Activity>, IntMap<Attribute, AttributeValue>>
{
    fn from(value: &'a Vec<(Vec<Activity>, IntMap<Attribute, AttributeValue>)>) -> Self {
        Self(value.iter())
    }
}

impl<'a, K, V> Iterator for VecTupleIterator<'a, K, V> {
    type Item = &'a K;

    fn next(&mut self) -> Option<Self::Item> {
        Some(&self.0.next()?.0)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }

    fn count(self) -> usize
    where
        Self: Sized,
    {
        self.0.count()
    }

    fn last(self) -> Option<Self::Item>
    where
        Self: Sized,
    {
        Some(&self.0.last()?.0)
    }

    fn nth(&mut self, n: usize) -> Option<Self::Item> {
        Some(&self.0.nth(n)?.0)
    }

    fn for_each<F>(self, f: F)
    where
        Self: Sized,
        F: FnMut(Self::Item),
    {
        #[inline]
        fn call<T>(mut f: impl FnMut(T)) -> impl FnMut((), T) {
            move |(), item| f(item)
        }

        self.fold((), call(f));
    }

    fn by_ref(&mut self) -> &mut Self
    where
        Self: Sized,
    {
        self
    }

    fn fold<B, F>(mut self, init: B, mut f: F) -> B
    where
        Self: Sized,
        F: FnMut(B, Self::Item) -> B,
    {
        let mut accum = init;
        while let Some(x) = self.next() {
            accum = f(accum, x);
        }
        accum
    }

    fn reduce<F>(mut self, f: F) -> Option<Self::Item>
    where
        Self: Sized,
        F: FnMut(Self::Item, Self::Item) -> Self::Item,
    {
        let first = self.next()?;
        Some(self.fold(first, f))
    }

    fn all<F>(&mut self, f: F) -> bool
    where
        Self: Sized,
        F: FnMut(Self::Item) -> bool,
    {
        #[inline]
        fn check<T>(
            mut f: impl FnMut(T) -> bool,
        ) -> impl FnMut((), T) -> std::ops::ControlFlow<()> {
            move |(), x| {
                if f(x) {
                    std::ops::ControlFlow::Continue(())
                } else {
                    std::ops::ControlFlow::Break(())
                }
            }
        }
        self.try_fold((), check(f)) == std::ops::ControlFlow::Continue(())
    }

    fn any<F>(&mut self, f: F) -> bool
    where
        Self: Sized,
        F: FnMut(Self::Item) -> bool,
    {
        #[inline]
        fn check<T>(
            mut f: impl FnMut(T) -> bool,
        ) -> impl FnMut((), T) -> std::ops::ControlFlow<()> {
            move |(), x| {
                if f(x) {
                    std::ops::ControlFlow::Break(())
                } else {
                    std::ops::ControlFlow::Continue(())
                }
            }
        }

        self.try_fold((), check(f)) == std::ops::ControlFlow::Break(())
    }

    fn find<P>(&mut self, predicate: P) -> Option<Self::Item>
    where
        Self: Sized,
        P: FnMut(&Self::Item) -> bool,
    {
        #[inline]
        fn check<T>(
            mut predicate: impl FnMut(&T) -> bool,
        ) -> impl FnMut((), T) -> std::ops::ControlFlow<T> {
            move |(), x| {
                if predicate(&x) {
                    std::ops::ControlFlow::Break(x)
                } else {
                    std::ops::ControlFlow::Continue(())
                }
            }
        }

        self.try_fold((), check(predicate)).break_value()
    }

    fn find_map<B, F>(&mut self, f: F) -> Option<B>
    where
        Self: Sized,
        F: FnMut(Self::Item) -> Option<B>,
    {
        #[inline]
        fn check<T, B>(
            mut f: impl FnMut(T) -> Option<B>,
        ) -> impl FnMut((), T) -> std::ops::ControlFlow<B> {
            move |(), x| match f(x) {
                Some(x) => std::ops::ControlFlow::Break(x),
                None => std::ops::ControlFlow::Continue(()),
            }
        }

        self.try_fold((), check(f)).break_value()
    }

    fn unzip<A, B, FromA, FromB>(self) -> (FromA, FromB)
    where
        FromA: Default + Extend<A>,
        FromB: Default + Extend<B>,
        Self: Sized + Iterator<Item = (A, B)>,
    {
        let mut unzipped: (FromA, FromB) = Default::default();
        unzipped.extend(self);
        unzipped
    }

    fn sum<S>(self) -> S
    where
        Self: Sized,
        S: std::iter::Sum<Self::Item>,
    {
        std::iter::Sum::sum(self)
    }

    fn product<P>(self) -> P
    where
        Self: Sized,
        P: std::iter::Product<Self::Item>,
    {
        std::iter::Product::product(self)
    }

    fn is_sorted(self) -> bool
    where
        Self: Sized,
        Self::Item: PartialOrd,
    {
        self.is_sorted_by(|a, b| a <= b)
    }

    fn is_sorted_by<F>(mut self, compare: F) -> bool
    where
        Self: Sized,
        F: FnMut(&Self::Item, &Self::Item) -> bool,
    {
        #[inline]
        fn check<'a, T>(
            last: &'a mut T,
            mut compare: impl FnMut(&T, &T) -> bool + 'a,
        ) -> impl FnMut(T) -> bool + 'a {
            move |curr| {
                if !compare(&last, &curr) {
                    return false;
                }
                *last = curr;
                true
            }
        }

        let mut last = match self.next() {
            Some(e) => e,
            None => return true,
        };

        self.all(check(&mut last, compare))
    }
}
