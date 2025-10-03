use crate::{
    EventLog, EventLogTraceAttributes, ebi_objects::compressed_event_log::CompressedEventLog,
};

impl From<EventLog> for CompressedEventLog {
    fn from(value: EventLog) -> Self {
        log::info!("Convert event log into compressed event log.");
        Self { log: value }
    }
}

impl From<EventLogTraceAttributes> for CompressedEventLog {
    fn from(value: EventLogTraceAttributes) -> Self {
        Self { log: value.into() }
    }
}
