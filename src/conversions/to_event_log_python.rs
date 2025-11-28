use crate::{EventLogXes, ebi_objects::event_log_python::EventLogPython};

impl From<process_mining::EventLog> for EventLogPython {
    fn from(value: process_mining::EventLog) -> Self {
        Self { log: value }
    }
}

impl From<EventLogXes> for EventLogPython {
    fn from(value: EventLogXes) -> Self {
        Self {
            log: value.rust4pm_log,
        }
    }
}