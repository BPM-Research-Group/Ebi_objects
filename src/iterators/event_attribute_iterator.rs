use crate::{
    Attribute, NumberOfTraces, ebi_objects::event_log_event_attributes::EventLogEventAttributes,
    traits::attributes::Attributes,
};
use chrono::{DateTime, FixedOffset};
use ebi_activity_key::Activity;
use ebi_arithmetic::Fraction;
use pastey::paste;

macro_rules! iterator {
    ($t:expr, $u:expr, $v:ty) => {
        paste! {
            pub struct [<$t EventAttributeIterator>]<'a> {
                log: &'a EventLogEventAttributes,
                attribute: Attribute,
                next: usize,
            }

            impl<'a> Iterator for [<$t EventAttributeIterator>]<'a> {
                type Item = [<$t EventAttributeIteratorEvent>]<'a>;

                fn next(&mut self) -> Option<Self::Item> {
                    let result = if self.next < self.log.number_of_traces() {
                        Some([<$t EventAttributeIteratorEvent>] {
                            log: self.log,
                            attribute: self.attribute,
                            trace: self.next,
                            next: 0,
                        })
                    } else {
                        None
                    };
                    self.next += 1;
                    result
                }
            }

            impl<'a> From<(&'a EventLogEventAttributes, Attribute)> for [<$t EventAttributeIterator>]<'a> {
                fn from(value: (&'a EventLogEventAttributes, Attribute)) -> Self {
                    Self {
                        log: value.0,
                        attribute: value.1,
                        next: 0,
                    }
                }
            }

            pub struct [<$t EventAttributeIteratorEvent>]<'a> {
                log: &'a EventLogEventAttributes,
                attribute: Attribute,
                trace: usize,
                next: usize,
            }

            impl<'a> Iterator for [<$t EventAttributeIteratorEvent>]<'a> {
                type Item = (Activity, Option<$v>);

                fn next(&mut self) -> Option<Self::Item> {
                    let trace = self.log.traces.get(self.trace)?;
                    let activity = trace.0.get(self.next)?;
                    let attribute_value = trace.1.get(self.next)?.[<get_attribute_ $u >](self.attribute);
                    self.next += 1;
                    Some((*activity, attribute_value))
                }
            }
        }
    };
}

iterator!(Categorical, categorical, String);
iterator!(Numeric, numeric, Fraction);
iterator!(Time, time, DateTime<FixedOffset>);
