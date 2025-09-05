use std::collections::HashSet;

use fnv::FnvBuildHasher;

use crate::{
    ebi_objects::{
        event_log::EventLog, finite_language::FiniteLanguage,
        finite_stochastic_language::FiniteStochasticLanguage,
    }, Activity, ActivityKey
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
