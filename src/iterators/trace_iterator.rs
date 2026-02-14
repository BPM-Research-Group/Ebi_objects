use std::collections::VecDeque;

use crate::{
    Activity, Attribute, HasActivityKey,
    ebi_objects::{
        event_log_csv::EventLogCsv, event_log_ocel::EventLogOcel, event_log_xes::EventLogXes,
    },
};
use intmap::IntMap;

/// An iterator over traces, where the traces do not exist in memory untill they are necessary.
pub enum TraceIterator<'a> {
    Csv(TraceIteratorCsv<'a>),
    Xes(TraceIteratorXes<'a>),
    Ocel(TraceIteratorOcel<'a>),
}

impl<'a> Iterator for TraceIterator<'a> {
    type Item = Vec<Activity>;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            TraceIterator::Csv(it) => it.next(),
            TraceIterator::Xes(it) => it.next(),
            TraceIterator::Ocel(it) => it.next(),
        }
    }
}

// === CSV ===

pub struct TraceIteratorCsv<'a> {
    log: &'a EventLogCsv,
    it: std::slice::Iter<'a, (String, Vec<IntMap<Attribute, String>>)>,
}

impl<'a> Iterator for TraceIteratorCsv<'a> {
    type Item = Vec<Activity>;

    fn next(&mut self) -> Option<Self::Item> {
        let (_, trace) = self.it.next()?;
        let mut result = Vec::with_capacity(trace.len());
        for event in trace.iter() {
            let empty = String::new();
            let activity_label = event
                .get(self.log.activity_attribute)
                .unwrap_or_else(|| &empty);
            let activity = self
                .log
                .activity_key
                .process_activity_attempt(activity_label)?;
            result.push(activity);
        }
        Some(result)
    }
}

impl<'a> From<&'a EventLogCsv> for TraceIteratorCsv<'a> {
    fn from(value: &'a EventLogCsv) -> Self {
        Self {
            log: value,
            it: value.traces.iter(),
        }
    }
}

// === XES ===

pub struct TraceIteratorXes<'a> {
    log: &'a EventLogXes,
    next: usize,
}

impl<'a> Iterator for TraceIteratorXes<'a> {
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

impl<'a> From<&'a EventLogXes> for TraceIteratorXes<'a> {
    fn from(value: &'a EventLogXes) -> Self {
        Self {
            log: value,
            next: 0,
        }
    }
}

// === OCEL ===

pub struct TraceIteratorOcel<'a> {
    log: &'a EventLogOcel,
    objects: VecDeque<String>,
}

impl<'a> Iterator for TraceIteratorOcel<'a> {
    type Item = Vec<Activity>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(object_id) = self.objects.pop_front() {
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

impl<'a> From<&'a EventLogOcel> for TraceIteratorOcel<'a> {
    fn from(value: &'a EventLogOcel) -> Self {
        let objects =
            EventLogOcel::get_relevant_objects(&value.rust4pm_log.objects, &value.object_type)
                .into_iter()
                .collect::<VecDeque<_>>();
        Self {
            log: value,
            objects,
        }
    }
}
