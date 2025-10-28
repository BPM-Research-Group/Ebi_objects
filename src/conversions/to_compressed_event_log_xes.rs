use crate::{CompressedEventLogXes, EventLogXes};

impl From<EventLogXes> for CompressedEventLogXes {
    fn from(value: EventLogXes) -> Self {
        log::info!("Convert XES event log into XES compressed event log.");
        Self { log: value }
    }
}
