use crate::{
    AttributeKey, CompressedEventLog, CompressedEventLogXes, EventLog, EventLogTraceAttributes,
    EventLogXes,
    ebi_objects::{
        compressed_event_log_trace_attributes::CompressedEventLogTraceAttributes,
        event_log_csv::{self, DEFAULT_QUOTE_CHARACTER, DEFAULT_SEPARATOR, EventLogCsv},
    },
};
use anyhow::{Error, anyhow};
use intmap::IntMap;
use process_mining::core::event_data::case_centric::{AttributeValue, XESEditableAttribute};

impl TryFrom<EventLog> for EventLogCsv {
    type Error = Error;

    fn try_from(value: EventLog) -> Result<Self, Self::Error> {
        let EventLog {
            activity_key,
            traces,
        } = value;

        //create the attribute key
        let mut attribute_key = AttributeKey::new();
        let trace_id_attribute = attribute_key.process_attribute_column(0, "0");
        let activity_attribute = attribute_key.process_attribute_column(1, "a");

        //transform the traces
        let mut data = Vec::with_capacity(traces.len());
        for (trace_id, trace) in traces.into_iter().enumerate() {
            if trace.len() == 0 {
                return Err(anyhow!(
                    "cannot convert this event log to csv; a csv file does not support empty traces"
                ));
            }

            let mut new_events = Vec::with_capacity(trace.len());
            for activity in trace {
                let mut map = IntMap::with_capacity(1);
                map.insert(trace_id_attribute, trace_id.to_string());
                map.insert(
                    activity_attribute,
                    activity_key.get_activity_label(&activity).to_string(),
                );
                new_events.push(map);
            }
            data.push((trace_id.to_string(), new_events));
        }

        Ok(EventLogCsv {
            activity_attribute,
            activity_key,
            attribute_key,
            traces: data,
            separator: event_log_csv::DEFAULT_SEPARATOR.as_bytes()[0],
            quote_character: event_log_csv::DEFAULT_QUOTE_CHARACTER.as_bytes()[0],
        })
    }
}

macro_rules! via_log {
    ($t:ident) => {
        impl TryFrom<$t> for EventLogCsv {
            type Error = Error;

            fn try_from(value: $t) -> Result<Self, Self::Error> {
                let log: EventLog = value.into();
                log.try_into()
            }
        }
    };
}

via_log!(CompressedEventLog);
via_log!(CompressedEventLogTraceAttributes);
via_log!(EventLogTraceAttributes);

impl TryFrom<EventLogXes> for EventLogCsv {
    type Error = Error;

    fn try_from(value: EventLogXes) -> Result<Self, Self::Error> {
        let EventLogXes {
            activity_key,
            classifier,
            rust4pm_log,
        } = value;

        //create the attribute key
        let mut attribute_key = AttributeKey::new();

        //transform the traces and events
        let mut traces = Vec::with_capacity(rust4pm_log.traces.len());
        for old_trace in rust4pm_log.traces {
            if old_trace.events.len() == 0 {
                return Err(anyhow!(
                    "cannot convert this XES event log to csv; a csv file does not support empty traces"
                ));
            }

            //get trace id
            let trace_id= attribute_to_string(old_trace.attributes.get_by_key("concept:name").ok_or_else(|| anyhow!("trace does not have a concept:name attribute, thus cannot be translated to CSV"))?.value.clone()).ok_or_else(|| anyhow!("trace has an invalid concept:name attribute, i.e. it cannot be transformed to a string"))?;

            //transform events
            let mut events = Vec::with_capacity(old_trace.events.len());
            for old_event in old_trace.events {
                let mut event = IntMap::new();
                for old_attribute in old_event.attributes {
                    let attribute = attribute_key
                        .process_attribute_value(&old_attribute.key, &old_attribute.value);
                    if let Some(attribute_value) = attribute_to_string(old_attribute.value) {
                        event.insert(attribute, attribute_value);
                    }
                }

                events.push(event);
            }

            traces.push((trace_id, events));
        }

        let activity_attribute = attribute_key
            .label_to_attribute(
                classifier
                    .keys
                    .iter()
                    .next()
                    .ok_or_else(|| anyhow!("no event classifier attribute defined"))?,
            )
            .ok_or_else(|| anyhow!("could not find event attribute"))?;

        Ok(EventLogCsv {
            activity_attribute,
            activity_key,
            attribute_key,
            traces,
            separator: DEFAULT_SEPARATOR.as_bytes()[0],
            quote_character: DEFAULT_QUOTE_CHARACTER.as_bytes()[0],
        })
    }
}

impl TryFrom<CompressedEventLogXes> for EventLogCsv {
    type Error = Error;

    fn try_from(value: CompressedEventLogXes) -> Result<Self, Self::Error> {
        let xes: EventLogXes = value.into();
        xes.try_into()
    }
}

fn attribute_to_string(value: AttributeValue) -> Option<String> {
    match value {
        AttributeValue::String(s) => Some(s.clone()),
        AttributeValue::Date(d) => Some(d.to_rfc3339()),
        AttributeValue::Int(i) => Some(i.to_string()),
        AttributeValue::Float(f) => Some(f.to_string()),
        AttributeValue::Boolean(b) => Some(b.to_string()),
        AttributeValue::ID(id) => Some(id.to_string()),
        AttributeValue::List(_) => None,
        AttributeValue::Container(_) => None,
        AttributeValue::None() => None,
    }
}

#[cfg(test)]
mod tests {
    use std::fs;

    use crate::{EventLog, EventLogXes, NumberOfTraces, ebi_objects::event_log_csv::EventLogCsv};

    #[test]
    fn csv_conversion() {
        let fin = fs::read_to_string("testfiles/a-b.xes").unwrap();
        let log: EventLog = fin.parse::<EventLog>().unwrap();
        assert_eq!(log.number_of_traces(), 2);

        let csv: EventLogCsv = log.try_into().unwrap();
        assert_eq!(csv.number_of_traces(), 2);
    }

    #[test]
    fn csv_xes_conversion() {
        let fin = fs::read_to_string("testfiles/a-b_multiple_separators.csv").unwrap();
        let csv: EventLogCsv = fin.parse::<EventLogCsv>().unwrap();
        assert_eq!(csv.number_of_traces(), 1);
        assert_eq!(csv.number_of_events(), 2);

        let xes: EventLogXes = csv.try_into().unwrap();
        assert_eq!(xes.number_of_traces(), 1);
        assert_eq!(xes.number_of_events(), 2);
    }
}
