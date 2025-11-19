use crate::{
    CompressedEventLogXes, EventLog, EventLogTraceAttributes, EventLogXes,
    ebi_objects::{
        compressed_event_log::CompressedEventLog,
        compressed_event_log_trace_attributes::CompressedEventLogTraceAttributes,
    },
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
