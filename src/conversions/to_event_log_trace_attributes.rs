use crate::{EventLogTraceAttributes, ebi_objects::compressed_event_log::CompressedEventLog};

impl From<CompressedEventLog> for EventLogTraceAttributes {
    fn from(value: CompressedEventLog) -> Self {
        log::info!("Convert compressed event log into event log.");
        value.log
    }
}
