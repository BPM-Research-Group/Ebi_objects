use crate::{Activity, EventLogTraceAttributes};

/// An iterator over traces.
pub struct TraceIterator<'a> {
    log: &'a EventLogTraceAttributes,
    next: usize,
}

impl<'a> Iterator for TraceIterator<'a> {
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

impl<'a> From<&'a EventLogTraceAttributes> for TraceIterator<'a> {
    fn from(value: &'a EventLogTraceAttributes) -> Self {
        Self {
            log: value,
            next: 0,
        }
    }
}
