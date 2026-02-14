use crate::{
    Activity, ActivityKey, CompressedEventLog, CompressedEventLogXes, EventLogOcel,
    EventLogTraceAttributes, EventLogXes,
    ebi_objects::{
        compressed_event_log_trace_attributes::CompressedEventLogTraceAttributes,
        event_log::EventLog, event_log_csv::EventLogCsv, event_log_python::EventLogPython,
        finite_stochastic_language::FiniteStochasticLanguage,
    },
};
use ebi_arithmetic::{Fraction, One};
use std::collections::{HashMap, hash_map::Entry};

impl From<EventLog> for FiniteStochasticLanguage {
    fn from(value: EventLog) -> Self {
        log::info!("create stochastic language");
        let mut map = HashMap::new();

        let EventLog {
            activity_key,
            traces,
            ..
        } = value;

        for trace in traces {
            match map.entry(trace) {
                Entry::Occupied(mut e) => {
                    *e.get_mut() += Fraction::one();
                    ()
                }
                Entry::Vacant(e) => {
                    e.insert(Fraction::one());
                    ()
                }
            }
        }

        (activity_key, map).into()
    }
}

impl From<HashMap<Vec<String>, Fraction>> for FiniteStochasticLanguage {
    /**
     * Normalises the distribution. Use new_raw to avoid normalisation.
     */
    fn from(mut value: HashMap<Vec<String>, Fraction>) -> Self {
        Self::normalise_before(&mut value);
        let mut activity_key = ActivityKey::new();
        let a_traces = value
            .into_iter()
            .map(|(trace, probability)| (activity_key.process_trace(&trace), probability))
            .collect();
        Self {
            activity_key: activity_key,
            traces: a_traces,
        }
    }
}

impl From<(ActivityKey, HashMap<Vec<Activity>, Fraction>)> for FiniteStochasticLanguage {
    /**
     * Normalises the distribution. Use new_raw to avoid normalisation.
     */
    fn from(value: (ActivityKey, HashMap<Vec<Activity>, Fraction>)) -> Self {
        let mut result = Self {
            activity_key: value.0,
            traces: value.1,
        };
        result.normalise();
        result
    }
}

macro_rules! from_via_log {
    ($t:ident) => {
        impl From<$t> for FiniteStochasticLanguage {
            fn from(value: $t) -> Self {
                let log: EventLog = value.into();
                log.into()
            }
        }
    };
}

from_via_log!(EventLogCsv);
from_via_log!(EventLogOcel);
from_via_log!(EventLogXes);
from_via_log!(EventLogPython);
from_via_log!(CompressedEventLog);
from_via_log!(CompressedEventLogXes);
from_via_log!(CompressedEventLogTraceAttributes);
from_via_log!(EventLogTraceAttributes);
