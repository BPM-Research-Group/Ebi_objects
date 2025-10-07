use crate::{traits::trace_attributes::TraceAttributes, Activity, Attribute, EventLogTraceAttributes, NumberOfTraces};

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

pub struct EventLogTraceAttributeCategoricalIterator<'a> {
    log: &'a EventLogTraceAttributes,
    attribute: Attribute,
    next: usize,
}

impl<'a> Iterator for EventLogTraceAttributeCategoricalIterator<'a> {
    type Item = Option<String>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.next >= self.log.number_of_traces() {
            return None;
        }
        let result = Some(
            self.log
                .get_trace_attribute_categorical(self.next, self.attribute),
        );

        self.next += 1;
        result
    }
}

impl<'a> From<(&'a EventLogTraceAttributes, Attribute)>
    for EventLogTraceAttributeCategoricalIterator<'a>
{
    fn from(value: (&'a EventLogTraceAttributes, Attribute)) -> Self {
        Self {
            log: value.0,
            attribute: value.1,
            next: 0,
        }
    }
}

pub type EventLogTraceAttributeTraceCategoricalIterator<'a> =
    std::iter::Zip<TraceIterator<'a>, EventLogTraceAttributeCategoricalIterator<'a>>;
