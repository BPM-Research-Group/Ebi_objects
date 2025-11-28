use process_mining::event_log::event_log_struct::EventLogClassifier;

use crate::{EventLogXes, ebi_objects::event_log_python::EventLogPython};

impl From<(process_mining::EventLog, EventLogClassifier)> for EventLogPython {
    fn from(value: (process_mining::EventLog, EventLogClassifier)) -> Self {
        Self { log: value.into() }
    }
}

impl From<EventLogXes> for EventLogPython {
    fn from(value: EventLogXes) -> Self {
        Self { log: value }
    }
}
