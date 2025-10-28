use ebi_arithmetic::Fraction;
use intmap::IntMap;
use process_mining::event_log::AttributeValue;
use rayon::iter::{
    IndexedParallelIterator, ParallelIterator,
    plumbing::{Consumer, Producer, ProducerCallback, UnindexedConsumer, bridge},
};
use std::collections::{HashMap, HashSet};

use crate::{Activity, Attribute};

/**
 * A parallel iterator over references to traces.
 *
 * As Rust doesn't allow generic types in traits, this is an enum.
 */
pub enum ParallelRefTraceIterator<'a> {
    Vec(rayon::slice::Iter<'a, Vec<Activity>>),
    VecTupleHashMap(ParallelVecTupleIterator<'a, Vec<Activity>, HashMap<String, u64>>),
    VecTupleIntMap(ParallelVecTupleIterator<'a, Vec<Activity>, IntMap<Attribute, AttributeValue>>),
    HashSet(ParallelHashSetIterator<'a, Vec<Activity>>),
    HashMap(ParallelHashMapKeysIterator<'a, Vec<Activity>, Fraction>),
}

impl<'a> ParallelIterator for ParallelRefTraceIterator<'a> {
    type Item = &'a Vec<Activity>;

    fn drive_unindexed<C>(self, consumer: C) -> C::Result
    where
        C: rayon::iter::plumbing::UnindexedConsumer<Self::Item>,
    {
        match self {
            ParallelRefTraceIterator::Vec(iter) => iter.drive_unindexed(consumer),
            ParallelRefTraceIterator::VecTupleHashMap(iter) => iter.drive_unindexed(consumer),
            ParallelRefTraceIterator::VecTupleIntMap(iter) => iter.drive_unindexed(consumer),
            ParallelRefTraceIterator::HashSet(iter) => iter.drive_unindexed(consumer),
            ParallelRefTraceIterator::HashMap(iter) => iter.drive_unindexed(consumer),
        }
    }

    fn opt_len(&self) -> Option<usize> {
        match self {
            ParallelRefTraceIterator::Vec(iter) => iter.opt_len(),
            ParallelRefTraceIterator::VecTupleHashMap(iter) => iter.opt_len(),
            ParallelRefTraceIterator::VecTupleIntMap(iter) => iter.opt_len(),
            ParallelRefTraceIterator::HashSet(iter) => iter.opt_len(),
            ParallelRefTraceIterator::HashMap(iter) => iter.opt_len(),
        }
    }
}

impl<'a> IndexedParallelIterator for ParallelRefTraceIterator<'a> {
    fn len(&self) -> usize {
        match self {
            ParallelRefTraceIterator::Vec(iter) => iter.len(),
            ParallelRefTraceIterator::VecTupleHashMap(iter) => iter.len(),
            ParallelRefTraceIterator::VecTupleIntMap(iter) => iter.len(),
            ParallelRefTraceIterator::HashSet(iter) => iter.len(),
            ParallelRefTraceIterator::HashMap(iter) => iter.len(),
        }
    }

    fn drive<C: Consumer<Self::Item>>(self, consumer: C) -> C::Result {
        bridge(self, consumer)
    }

    fn with_producer<CB: ProducerCallback<Self::Item>>(self, callback: CB) -> CB::Output {
        match self {
            ParallelRefTraceIterator::Vec(iter) => iter.with_producer(callback),
            ParallelRefTraceIterator::VecTupleHashMap(iter) => iter.with_producer(callback),
            ParallelRefTraceIterator::VecTupleIntMap(iter) => iter.with_producer(callback),
            ParallelRefTraceIterator::HashSet(iter) => iter.with_producer(callback),
            ParallelRefTraceIterator::HashMap(iter) => iter.with_producer(callback),
        }
    }
}

pub struct ParallelHashSetIterator<'a, K> {
    iter: std::collections::hash_set::Iter<'a, K>,
}

impl<'a, K, T> From<&'a HashSet<K, T>> for ParallelHashSetIterator<'a, K> {
    fn from(value: &'a HashSet<K, T>) -> Self {
        Self { iter: value.iter() }
    }
}

impl<'a, K: Sync> ParallelIterator for ParallelHashSetIterator<'a, K> {
    type Item = &'a K;

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

impl<'a, K: Sync> IndexedParallelIterator for ParallelHashSetIterator<'a, K> {
    fn with_producer<CB: ProducerCallback<Self::Item>>(self, callback: CB) -> CB::Output {
        let producer = ParallelHashSetIteratorProducer::from(self);
        callback.callback(producer)
    }

    fn drive<C: Consumer<Self::Item>>(self, consumer: C) -> C::Result {
        bridge(self, consumer)
    }

    fn len(&self) -> usize {
        self.iter.len()
    }
}

struct ParallelHashSetIteratorProducer<'a, K> {
    iter: std::collections::hash_set::Iter<'a, K>,
    min: usize,
    max: usize,
}

impl<'a, K> From<ParallelHashSetIterator<'a, K>> for ParallelHashSetIteratorProducer<'a, K> {
    fn from(iterator: ParallelHashSetIterator<'a, K>) -> Self {
        let len = iterator.iter.len();
        Self {
            iter: iterator.iter,
            min: 0,
            max: len,
        }
    }
}

impl<'a, K: Sync> Producer for ParallelHashSetIteratorProducer<'a, K> {
    type Item = &'a K;
    type IntoIter = ParallelHashSetIteratorIterator<'a, K>;

    fn into_iter(self) -> Self::IntoIter {
        ParallelHashSetIteratorIterator::new(self.iter, self.min, self.max)
    }

    fn split_at(self, index: usize) -> (Self, Self) {
        let split = self.min + index;
        (
            ParallelHashSetIteratorProducer {
                iter: self.iter.clone(),
                min: self.min,
                max: split,
            },
            ParallelHashSetIteratorProducer {
                iter: self.iter,
                min: split,
                max: self.max,
            },
        )
    }
}

struct ParallelHashSetIteratorIterator<'a, K> {
    keys: std::collections::hash_set::Iter<'a, K>,
    left: usize,
}

impl<'a, K> ParallelHashSetIteratorIterator<'a, K> {
    fn new(mut keys: std::collections::hash_set::Iter<'a, K>, min: usize, max: usize) -> Self {
        if min > 0 {
            keys.nth(min - 1);
        }
        Self {
            keys,
            left: max - min,
        }
    }
}

impl<'a, K> Iterator for ParallelHashSetIteratorIterator<'a, K> {
    type Item = &'a K;

    fn next(&mut self) -> Option<Self::Item> {
        if self.left == 0 {
            None
        } else {
            let result = self.keys.next();
            self.left -= 1;
            result
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.left, Some(self.left))
    }
}

impl<'a, K> ExactSizeIterator for ParallelHashSetIteratorIterator<'a, K> {
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

impl<'a, K> DoubleEndedIterator for ParallelHashSetIteratorIterator<'a, K> {
    fn next_back(&mut self) -> Option<Self::Item> {
        if self.left == 0 {
            None
        } else {
            let mut keys = self.keys.clone();
            let result = keys.nth(self.left);
            self.left -= 1;
            result
        }
    }
}

/// In order to iterate over the keys of a hashmap in parallel,
/// we first need a struct that is the parallel iterator.
pub struct ParallelHashMapKeysIterator<'a, K, V> {
    keys: std::collections::hash_map::Keys<'a, K, V>,
}

/// The parallel iterator can be created from a reference to a hashmap.
impl<'a, K, V> From<&'a HashMap<K, V>> for ParallelHashMapKeysIterator<'a, K, V> {
    fn from(value: &'a HashMap<K, V>) -> Self {
        Self { keys: value.keys() }
    }
}

/// We implement the ParallelIterator trait for our parallel keys iterator.
impl<'a, K: Sync, V: Sync> ParallelIterator for ParallelHashMapKeysIterator<'a, K, V> {
    type Item = &'a K;

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

/// In order for the bridge function to work,
/// we need to implement IndexedParallelIterator for our parallel keys iterator.
impl<'a, K: Sync, V: Sync> IndexedParallelIterator for ParallelHashMapKeysIterator<'a, K, V> {
    fn with_producer<CB: ProducerCallback<Self::Item>>(self, callback: CB) -> CB::Output {
        let producer = KeysIteratorDataProducer::from(self);
        callback.callback(producer)
    }

    fn drive<C: Consumer<Self::Item>>(self, consumer: C) -> C::Result {
        bridge(self, consumer)
    }

    fn len(&self) -> usize {
        self.keys.len()
    }
}

/// We need an iterator similar to a slice iterator, but then one that works on the Keys trait.
struct KeysIterator<'a, K, V> {
    keys: std::collections::hash_map::Keys<'a, K, V>,
    left: usize,
}

impl<'a, K, V> KeysIterator<'a, K, V> {
    fn new(mut keys: std::collections::hash_map::Keys<'a, K, V>, min: usize, max: usize) -> Self {
        if min > 0 {
            keys.nth(min - 1);
        }
        Self {
            keys,
            left: max - min,
        }
    }
}

impl<'a, K, V> Iterator for KeysIterator<'a, K, V> {
    type Item = &'a K;

    fn next(&mut self) -> Option<Self::Item> {
        if self.left == 0 {
            None
        } else {
            let result = self.keys.next();
            self.left -= 1;
            result
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.left, Some(self.left))
    }
}

impl<'a, K, V> ExactSizeIterator for KeysIterator<'a, K, V> {
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

impl<'a, K, V> DoubleEndedIterator for KeysIterator<'a, K, V> {
    fn next_back(&mut self) -> Option<Self::Item> {
        if self.left == 0 {
            None
        } else {
            let mut keys = self.keys.clone();
            let result = keys.nth(self.left);
            self.left -= 1;
            result
        }
    }
}

/// Rayon uses a splittable iterator, which is our "data producer".
struct KeysIteratorDataProducer<'a, K, V> {
    keys: std::collections::hash_map::Keys<'a, K, V>,
    min: usize,
    max: usize,
}

/// We need to implement the Producer trait for our data producer.
impl<'a, K: Sync, V: Sync> Producer for KeysIteratorDataProducer<'a, K, V> {
    type Item = &'a K;
    type IntoIter = KeysIterator<'a, K, V>;

    fn into_iter(self) -> Self::IntoIter {
        KeysIterator::new(self.keys, self.min, self.max)
    }

    fn split_at(self, index: usize) -> (Self, Self) {
        let split = self.min + index;
        (
            KeysIteratorDataProducer {
                keys: self.keys.clone(),
                min: self.min,
                max: split,
            },
            KeysIteratorDataProducer {
                keys: self.keys,
                min: split,
                max: self.max,
            },
        )
    }
}

impl<'a, K, V> From<ParallelHashMapKeysIterator<'a, K, V>> for KeysIteratorDataProducer<'a, K, V> {
    fn from(iterator: ParallelHashMapKeysIterator<'a, K, V>) -> Self {
        let len = iterator.keys.len();
        Self {
            keys: iterator.keys,
            min: 0,
            max: len,
        }
    }
}

pub struct ParallelVecTupleIterator<'a, K, V> {
    iter: std::slice::Iter<'a, (K, V)>,
}

impl<'a, K, V> From<&'a Vec<(K, V)>> for ParallelVecTupleIterator<'a, K, V> {
    fn from(value: &'a Vec<(K, V)>) -> Self {
        Self { iter: value.iter() }
    }
}

impl<'a, K: Sync, V: Sync> ParallelIterator for ParallelVecTupleIterator<'a, K, V> {
    type Item = &'a K;

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

impl<'a, K: Sync, V: Sync> IndexedParallelIterator for ParallelVecTupleIterator<'a, K, V> {
    fn with_producer<CB: ProducerCallback<Self::Item>>(self, callback: CB) -> CB::Output {
        let producer = ParallelVecTupleIteratorProducer::from(self);
        callback.callback(producer)
    }

    fn drive<C: Consumer<Self::Item>>(self, consumer: C) -> C::Result {
        bridge(self, consumer)
    }

    fn len(&self) -> usize {
        self.iter.len()
    }
}

struct ParallelVecTupleIteratorProducer<'a, K, V> {
    iter: std::slice::Iter<'a, (K, V)>,
    min: usize,
    max: usize,
}

/// We need to implement the Producer trait for our data producer.
impl<'a, K: Sync + 'a, V: Sync> Producer for ParallelVecTupleIteratorProducer<'a, K, V> {
    type Item = &'a K;
    type IntoIter = ParallelVecTupleIteratorIterator<'a, K, V>;

    fn into_iter(self) -> Self::IntoIter {
        ParallelVecTupleIteratorIterator::new(self.iter, self.min, self.max)
    }

    fn split_at(self, index: usize) -> (Self, Self) {
        let split = self.min + index;
        (
            ParallelVecTupleIteratorProducer {
                iter: self.iter.clone(),
                min: self.min,
                max: split,
            },
            ParallelVecTupleIteratorProducer {
                iter: self.iter,
                min: split,
                max: self.max,
            },
        )
    }
}

impl<'a, K, V> From<ParallelVecTupleIterator<'a, K, V>>
    for ParallelVecTupleIteratorProducer<'a, K, V>
{
    fn from(iterator: ParallelVecTupleIterator<'a, K, V>) -> Self {
        let len = iterator.iter.len();
        Self {
            iter: iterator.iter,
            min: 0,
            max: len,
        }
    }
}

#[derive(Clone)]
struct ParallelVecTupleIteratorIterator<'a, K, V> {
    keys: std::slice::Iter<'a, (K, V)>,
    left: usize,
}

impl<'a, K, V> ParallelVecTupleIteratorIterator<'a, K, V> {
    fn new(mut keys: std::slice::Iter<'a, (K, V)>, min: usize, max: usize) -> Self {
        if min > 0 {
            keys.nth(min - 1);
        }
        Self {
            keys,
            left: max - min,
        }
    }
}

impl<'a, K, V> Iterator for ParallelVecTupleIteratorIterator<'a, K, V> {
    type Item = &'a K;

    fn next(&mut self) -> Option<Self::Item> {
        if self.left == 0 {
            None
        } else {
            let result = &self.keys.next()?.0;
            self.left -= 1;
            Some(&result)
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.left, Some(self.left))
    }
}

impl<'a, K, V> ExactSizeIterator for ParallelVecTupleIteratorIterator<'a, K, V> {
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

impl<'a, K, V> DoubleEndedIterator for ParallelVecTupleIteratorIterator<'a, K, V> {
    fn next_back(&mut self) -> Option<Self::Item> {
        if self.left == 0 {
            None
        } else {
            let mut keys = self.keys.clone();
            let result = &keys.nth(self.left)?.0;
            self.left -= 1;
            Some(result)
        }
    }
}

#[cfg(test)]
mod tests {
    use rayon::iter::ParallelIterator;
    use std::collections::HashMap;

    use crate::iterators::parallel_ref_trace_iterator::{
        KeysIterator, ParallelHashMapKeysIterator,
    };

    #[test]
    fn keys_iterator() {
        let mut umap = HashMap::new();
        for i in 10..100 {
            umap.insert(i, i + 200);
        }

        assert_eq!(umap.len(), 90);

        let it = KeysIterator::new(umap.keys(), 0, umap.len());
        let result = it.map(|x| x + 1).collect::<Vec<_>>();

        assert_eq!(result.len(), 90);

        println!("{:?}", result);

        for i in 10..100 {
            assert!(result.contains(&(i + 1)));
        }
    }

    #[test]
    fn par_keys_iterator() {
        let mut umap = HashMap::new();
        for i in 10..100 {
            umap.insert(i, i + 200);
        }

        assert_eq!(umap.len(), 90);

        let it = ParallelHashMapKeysIterator::from(&umap);
        let result = it.map(|x| x + 1).collect::<Vec<_>>();

        assert_eq!(result.len(), 90);

        println!("{:?}", result);

        for i in 10..100 {
            assert!(result.contains(&(i + 1)));
        }
    }
}
