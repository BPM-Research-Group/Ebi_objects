use crate::{
    CompressedEventLogXes, EventLogCsv, EventLogOcel, EventLogXes,
    ebi_objects::{
        compressed_event_log_event_attributes::CompressedEventLogEventAttributes,
        event_log_event_attributes::EventLogEventAttributes,
    },
};

macro_rules! via {
    ($t:ident) => {
        impl From<$t> for CompressedEventLogEventAttributes {
            fn from(value: $t) -> Self {
                let log: EventLogEventAttributes = value.into();
                Self { log }
            }
        }
    };
}

via!(EventLogXes);
via!(EventLogEventAttributes);
via!(CompressedEventLogXes);
via!(EventLogCsv);
via!(EventLogOcel);
