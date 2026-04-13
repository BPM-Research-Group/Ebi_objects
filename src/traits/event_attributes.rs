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
}
