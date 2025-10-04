use chrono::{DateTime, FixedOffset};
use ebi_arithmetic::Fraction;

use crate::{Activity, Attribute};

pub trait IndexTraceAttributes {
    fn number_of_traces(&self) -> usize;

    fn iter(&self) -> impl Iterator<Item = Vec<Activity>>;

    fn get_trace_attribute_categorical(
        &self,
        trace_index: usize,
        attribute: Attribute,
    ) -> Option<String>;
    fn get_trace_attribute_time(
        &self,
        trace_index: usize,
        attribute: Attribute,
    ) -> Option<DateTime<FixedOffset>>;
    fn get_trace_attribute_numeric(
        &self,
        trace_index: usize,
        attribute: Attribute,
    ) -> Option<Fraction>;
}
