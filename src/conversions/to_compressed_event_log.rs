use anyhow::Error;

use crate::{
    CompressedEventLogXes, EventLog, EventLogCsv, EventLogTraceAttributes, EventLogXes, ebi_objects::{
        compressed_event_log::CompressedEventLog,
        compressed_event_log_trace_attributes::CompressedEventLogTraceAttributes,
    }
};

macro_rules! via {
    ($t:ident) => {
        impl From<$t> for CompressedEventLog {
            fn from(value: $t) -> Self {
                let log: EventLog = value.into();
                Self { log }
            }
        }
    };
}

via!(CompressedEventLogXes);
via!(CompressedEventLogTraceAttributes);
via!(EventLog);
via!(EventLogTraceAttributes);
via!(EventLogXes);

impl TryFrom<EventLogCsv> for CompressedEventLog {
    type Error = Error;

    fn try_from(value: EventLogCsv) -> Result<Self, Self::Error> {
        let xes: EventLogXes = value.try_into()?;
        Ok(xes.into())
    }
}