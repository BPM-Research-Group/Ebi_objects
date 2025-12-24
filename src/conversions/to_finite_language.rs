use std::collections::HashSet;

use fnv::FnvBuildHasher;

use crate::{
    Activity, ActivityKey, CompressedEventLog, CompressedEventLogXes, EventLogTraceAttributes, EventLogXes, ebi_objects::{
        compressed_event_log_trace_attributes::CompressedEventLogTraceAttributes, event_log::EventLog, event_log_csv::EventLogCsv, event_log_python::EventLogPython, finite_language::FiniteLanguage, finite_stochastic_language::FiniteStochasticLanguage
    }
};

impl From<EventLog> for FiniteLanguage {
    fn from(value: EventLog) -> Self {
        log::info!("convert event log to finite language");
        let EventLog {
            activity_key,
            traces,
            ..
        } = value;

        let map: HashSet<Vec<Activity>, FnvBuildHasher> = traces.into_iter().collect();

        FiniteLanguage::from((activity_key, map))
    }
}

macro_rules! from_via_log {
    ($t:ident) => {
        impl From<$t> for FiniteLanguage {
            fn from(value: $t) -> Self {
                let log: EventLog = value.into();
                log.into()
            }
        }
    };
}

from_via_log!(EventLogCsv);
from_via_log!(EventLogXes);
from_via_log!(EventLogPython);
from_via_log!(CompressedEventLog);
from_via_log!(CompressedEventLogXes);
from_via_log!(CompressedEventLogTraceAttributes);
from_via_log!(EventLogTraceAttributes);

impl From<FiniteStochasticLanguage> for FiniteLanguage {
    fn from(value: FiniteStochasticLanguage) -> Self {
        log::info!("convert finite stochastic language into finite language");

        let FiniteStochasticLanguage {
            activity_key,
            traces,
        } = value;
        let mut map = FiniteLanguage::new_hashmap();
        for (trace, _) in traces {
            map.insert(trace);
        }

        FiniteLanguage::from((activity_key, map))
    }
}

impl From<HashSet<Vec<String>>> for FiniteLanguage {
    fn from(value: HashSet<Vec<String>>) -> Self {
        let mut activity_key = ActivityKey::new();
        let traces = value
            .into_iter()
            .map(|trace| activity_key.process_trace(&trace))
            .collect();
        Self {
            activity_key: activity_key,
            traces: traces,
        }
    }
}

impl From<(ActivityKey, HashSet<Vec<Activity>, FnvBuildHasher>)> for FiniteLanguage {
    fn from(value: (ActivityKey, HashSet<Vec<Activity>, FnvBuildHasher>)) -> Self {
        Self {
            activity_key: value.0,
            traces: value.1,
        }
    }
}
