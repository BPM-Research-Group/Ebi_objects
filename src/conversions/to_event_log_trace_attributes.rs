use process_mining::event_log::event_log_struct::EventLogClassifier;

use crate::{
    ActivityKey, AttributeKey, EventLogTraceAttributes,
    ebi_objects::compressed_event_log_trace_attributes::CompressedEventLogTraceAttributes,
};

impl From<CompressedEventLogTraceAttributes> for EventLogTraceAttributes {
    fn from(value: CompressedEventLogTraceAttributes) -> Self {
        value.log
    }
}

impl From<(process_mining::EventLog, EventLogClassifier)> for EventLogTraceAttributes {
    fn from(value: (process_mining::EventLog, EventLogClassifier)) -> Self {
        let mut result = Self {
            classifier: value.1,
            rust4pm_log: value.0,
            activity_key: ActivityKey::new(),
            attribute_key: AttributeKey::new(),
            trace_cache: vec![],
        };
        result.process_activity_key();
        result
    }
}
