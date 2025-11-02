use crate::{
    Activity, Attribute,
    ebi_objects::{event_log_csv::EventLogCsv, event_log_xes::EventLogXes},
};
use intmap::IntMap;

/// An iterator over traces, where the traces do not exist in memory untill they are necessary.
pub enum TraceIterator<'a> {
    Csv(TraceIteratorCsv<'a>),
    Xes(TraceIteratorXes<'a>),
}

impl<'a> Iterator for TraceIterator<'a> {
    type Item = Vec<Activity>;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            TraceIterator::Csv(it) => it.next(),
            TraceIterator::Xes(it) => it.next(),
        }
    }
}

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
