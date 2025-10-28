use crate::{ActivityKey, CompressedEventLogXes, ebi_objects::event_log_xes::EventLogXes};
use process_mining::event_log::event_log_struct::EventLogClassifier;

impl From<CompressedEventLogXes> for EventLogXes {
    fn from(value: CompressedEventLogXes) -> Self {
        value.log
    }
}

impl From<(process_mining::EventLog, EventLogClassifier)> for EventLogXes {
    fn from(value: (process_mining::EventLog, EventLogClassifier)) -> Self {
        let mut result = Self {
            classifier: value.1,
            rust4pm_log: value.0,
            activity_key: ActivityKey::new(),
        };
        result.create_activity_key();
        result
    }
}
