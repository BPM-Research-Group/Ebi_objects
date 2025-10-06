use chrono::{DateTime, FixedOffset};
use ebi_arithmetic::Fraction;

use crate::{
    Attribute, AttributeKey, parallel_trace_iterator::ParallelEventLogTraceAttributesIterator,
    trace_iterator::EventLogTraceAttributesIterator,
};

pub trait IndexTraceAttributes {
    fn number_of_traces(&self) -> usize;

    fn iter_traces(&'_ self) -> EventLogTraceAttributesIterator<'_>;

    fn par_iter_traces(&self) -> ParallelEventLogTraceAttributesIterator<'_>;

    fn attribute_key(&self) -> &AttributeKey;

    fn attribute_key_mut(&mut self) -> &mut AttributeKey;

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
