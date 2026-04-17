use crate::Attribute;
use chrono::{DateTime, FixedOffset};
use ebi_arithmetic::Fraction;

pub trait EventAttributes {
    fn get_event_attribute_categorical(
        &self,
        trace_index: usize,
        event_index: usize,
        attribute: Attribute,
    ) -> Option<String>;

    fn get_event_attribute_time(
        &self,
        trace_index: usize,
        event_index: usize,
        attribute: Attribute,
    ) -> Option<DateTime<FixedOffset>>;

    fn get_event_attribute_numeric(
        &self,
        trace_index: usize,
        event_index: usize,
        attribute: Attribute,
    ) -> Option<Fraction>;

    /// Returns the resource of the given event, if the event and the trace exist, it has a resource attribute, and the attribute was declared correctly on import.
    fn get_event_resource(&self, trace_index: usize, event_index: usize) -> Option<&String>;

    /// Returns the time of the given event, if the event and the trace exist, it has a time-parseable attribute, and the attribute was declared correctly on import.
    fn get_event_time(
        &self,
        trace_index: usize,
        event_index: usize,
    ) -> Option<&DateTime<FixedOffset>>;
}
