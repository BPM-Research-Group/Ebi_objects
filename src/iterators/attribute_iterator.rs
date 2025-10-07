use chrono::{DateTime, FixedOffset};
use ebi_arithmetic::Fraction;

use crate::{Attribute, EventLogTraceAttributes, NumberOfTraces, TraceAttributes};

pub struct CategoricalAttributeIterator<'a> {
    log: &'a EventLogTraceAttributes,
    attribute: Attribute,
    next: usize,
}

impl<'a> Iterator for CategoricalAttributeIterator<'a> {
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

impl<'a> From<(&'a EventLogTraceAttributes, Attribute)> for CategoricalAttributeIterator<'a> {
    fn from(value: (&'a EventLogTraceAttributes, Attribute)) -> Self {
        Self {
            log: value.0,
            attribute: value.1,
            next: 0,
        }
    }
}

pub struct NumericAttributeIterator<'a> {
    log: &'a EventLogTraceAttributes,
    attribute: Attribute,
    next: usize,
}

impl<'a> Iterator for NumericAttributeIterator<'a> {
    type Item = Option<Fraction>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.next >= self.log.number_of_traces() {
            return None;
        }
        let result = Some(
            self.log
                .get_trace_attribute_numeric(self.next, self.attribute),
        );

        self.next += 1;
        result
    }
}

impl<'a> From<(&'a EventLogTraceAttributes, Attribute)> for NumericAttributeIterator<'a> {
    fn from(value: (&'a EventLogTraceAttributes, Attribute)) -> Self {
        Self {
            log: value.0,
            attribute: value.1,
            next: 0,
        }
    }
}

pub struct TimeAttributeIterator<'a> {
    log: &'a EventLogTraceAttributes,
    attribute: Attribute,
    next: usize,
}

impl<'a> Iterator for TimeAttributeIterator<'a> {
    type Item = Option<DateTime<FixedOffset>>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.next >= self.log.number_of_traces() {
            return None;
        }
        let result = Some(self.log.get_trace_attribute_time(self.next, self.attribute));

        self.next += 1;
        result
    }
}

impl<'a> From<(&'a EventLogTraceAttributes, Attribute)> for TimeAttributeIterator<'a> {
    fn from(value: (&'a EventLogTraceAttributes, Attribute)) -> Self {
        Self {
            log: value.0,
            attribute: value.1,
            next: 0,
        }
    }
}
