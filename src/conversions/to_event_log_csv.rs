use crate::{
    AttributeKey, CompressedEventLog, EventLog, EventLogTraceAttributes,
    ebi_objects::{
        compressed_event_log_trace_attributes::CompressedEventLogTraceAttributes,
        event_log_csv::{self, EventLogCsv},
    },
};
use anyhow::{Error, anyhow};
use intmap::IntMap;

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

#[cfg(test)]
mod tests {
    use std::fs;

    use crate::{EventLog, NumberOfTraces, ebi_objects::event_log_csv::EventLogCsv};

    #[test]
    fn csv_conversion() {
        let fin = fs::read_to_string("testfiles/a-b.xes").unwrap();
        let log: EventLog = fin.parse::<EventLog>().unwrap();
        assert_eq!(log.number_of_traces(), 2);

        let csv: EventLogCsv = log.try_into().unwrap();
        assert_eq!(csv.number_of_traces(), 2);
    }
}
