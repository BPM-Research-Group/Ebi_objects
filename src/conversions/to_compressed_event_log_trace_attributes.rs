use crate::{
    CompressedEventLogXes, EventLogTraceAttributes, EventLogXes,
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

impl From<EventLogXes> for CompressedEventLogTraceAttributes {
    fn from(value: EventLogXes) -> Self {
        Self { log: value.into() }
    }
}

impl From<CompressedEventLogXes> for CompressedEventLogTraceAttributes {
    fn from(value: CompressedEventLogXes) -> Self {
        Self {
            log: value.log.into(),
        }
    }
}
