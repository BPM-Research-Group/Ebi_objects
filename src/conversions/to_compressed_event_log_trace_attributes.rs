use crate::{
    EventLogTraceAttributes,
    ebi_objects::compressed_event_log_trace_attributes::CompressedEventLogTraceAttributes,
};

impl From<EventLogTraceAttributes> for CompressedEventLogTraceAttributes {
    fn from(value: EventLogTraceAttributes) -> Self {
        log::info!(
            "Convert event log with trace attributes into compressed event log with trace attributes."
        );
        Self { log: value }
    }
}
