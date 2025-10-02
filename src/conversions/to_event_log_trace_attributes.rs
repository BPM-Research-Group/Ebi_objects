use anyhow::{anyhow, Error};

use crate::{EventLogTraceAttributes, ebi_objects::compressed_event_log::CompressedEventLog};

impl TryFrom<CompressedEventLog> for EventLogTraceAttributes {
    type Error = Error;
    
    fn try_from(value: CompressedEventLog) -> Result<Self, Self::Error> {
        if let CompressedEventLog::EventLogTraceAttributes(log) = value {
            Ok(log)
        } else {
            Err(anyhow!("cannot transform an event log into an event log with trace attributes"))
        }
    }
}
