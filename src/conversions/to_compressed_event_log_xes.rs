use anyhow::Error;

use crate::{CompressedEventLogXes, EventLogCsv, EventLogXes};

impl From<EventLogXes> for CompressedEventLogXes {
    fn from(value: EventLogXes) -> Self {
        log::info!("Convert XES event log into XES compressed event log.");
        Self { log: value }
    }
}

impl TryFrom<EventLogCsv> for CompressedEventLogXes {
    type Error = Error;

    fn try_from(value: EventLogCsv) -> Result<Self, Self::Error> {
        let xes: EventLogXes = value.try_into()?;
        Ok(xes.into())
    }
}
