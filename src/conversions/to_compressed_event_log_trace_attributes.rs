use crate::{
    CompressedEventLogXes, EventLogTraceAttributes, EventLogXes,
    ebi_objects::compressed_event_log_trace_attributes::CompressedEventLogTraceAttributes,
};

macro_rules! via {
    ($t:ident) => {
        impl From<$t> for CompressedEventLogTraceAttributes {
            fn from(value: $t) -> Self {
                let log: EventLogTraceAttributes = value.into();
                Self { log }
            }
        }
    };
}

via!(EventLogXes);
via!(EventLogTraceAttributes);
via!(CompressedEventLogXes);