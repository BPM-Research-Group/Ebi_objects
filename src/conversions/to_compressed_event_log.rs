use crate::{
    CompressedEventLogXes, EventLog, EventLogTraceAttributes, EventLogXes,
    ebi_objects::{
        compressed_event_log::CompressedEventLog,
        compressed_event_log_trace_attributes::CompressedEventLogTraceAttributes,
    },
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

impl From<EventLogXes> for CompressedEventLog {
    fn from(value: EventLogXes) -> Self {
        Self { log: value.into() }
    }
}

impl From<CompressedEventLogTraceAttributes> for CompressedEventLog {
    fn from(value: CompressedEventLogTraceAttributes) -> Self {
        Self {
            log: value.log.into(),
        }
    }
}

impl From<CompressedEventLogXes> for CompressedEventLog {
    fn from(value: CompressedEventLogXes) -> Self {
        Self {
            log: value.log.into(),
        }
    }
}
