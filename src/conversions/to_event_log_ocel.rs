use crate::{ActivityKey, ebi_objects::event_log_ocel::EventLogOcel};

/// OCEL, object_type, resource attribute, timestamp attribute
impl From<(process_mining::OCEL, String, String, String)> for EventLogOcel {
    /// it is the responsibility of the caller to ensure the second argument is a valid object_type
    fn from(value: (process_mining::OCEL, String, String, String)) -> Self {
        let mut result = Self {
            rust4pm_log: value.0,
            activity_key: ActivityKey::new(),
            object_type: value.1,
            resource_attribute: value.2,
            time_attribute: value.3,
        };
        result.create_activity_key();
        result
    }
}
