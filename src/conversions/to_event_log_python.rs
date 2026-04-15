use crate::{
    EventLogXes,
    ebi_objects::{
        event_log_python::EventLogPython,
        event_log_xes::{XES_DEFAULT_PARAMETER_RESOURCE, XES_DEFAULT_PARAMETER_TIMESTAMP},
    },
};
use process_mining::core::event_data::case_centric::EventLogClassifier;

impl From<(process_mining::EventLog, EventLogClassifier)> for EventLogPython {
    fn from(value: (process_mining::EventLog, EventLogClassifier)) -> Self {
        Self {
            log: (
                value.0,
                value.1,
                XES_DEFAULT_PARAMETER_RESOURCE.to_string(),
                XES_DEFAULT_PARAMETER_TIMESTAMP.to_string(),
            )
                .into(),
        }
    }
}

impl From<EventLogXes> for EventLogPython {
    fn from(value: EventLogXes) -> Self {
        Self { log: value }
    }
}
