use crate::{
    Activity, ActivityKey, CompressedEventLogXes, EventLogTraceAttributes, EventLogXes,
    IntoTraceIterator,
    ebi_objects::{
        compressed_event_log::CompressedEventLog,
        compressed_event_log_trace_attributes::CompressedEventLogTraceAttributes,
        event_log::EventLog, event_log_csv::EventLogCsv, event_log_python::EventLogPython,
    },
};
use process_mining::event_log::event_log_struct::EventLogClassifier;

impl From<CompressedEventLog> for EventLog {
    fn from(value: CompressedEventLog) -> Self {
        log::info!("Convert compressed event log into event log.");
        value.log
    }
}

impl From<CompressedEventLogTraceAttributes> for EventLog {
    fn from(value: CompressedEventLogTraceAttributes) -> Self {
        log::info!("Convert compressed event log with trace attributes into event log.");
        value.log.into()
    }
}

impl From<EventLogTraceAttributes> for EventLog {
    fn from(value: EventLogTraceAttributes) -> Self {
        Self {
            activity_key: value.activity_key,
            traces: value.traces.into_iter().map(|(trace, _)| trace).collect(),
        }
    }
}

impl From<EventLogXes> for EventLog {
    fn from(value: EventLogXes) -> Self {
        log::info!("Convert XES event log into event log.");
        let traces = value.iter_traces().collect();
        Self {
            activity_key: value.activity_key,
            traces,
        }
    }
}

impl From<EventLogPython> for EventLog {
    fn from(value: EventLogPython) -> Self {
        EventLog::from((value.log, EventLogClassifier::default())).into()
    }
}

impl From<CompressedEventLogXes> for EventLog {
    fn from(value: CompressedEventLogXes) -> Self {
        value.log.into()
    }
}

impl From<EventLogCsv> for EventLog {
    fn from(value: EventLogCsv) -> Self {
        let traces = value.iter_traces().collect();
        Self {
            activity_key: value.activity_key,
            traces,
        }
    }
}

impl From<(process_mining::EventLog, EventLogClassifier)> for EventLog {
    fn from(value: (process_mining::EventLog, EventLogClassifier)) -> Self {
        let mut result = Self {
            activity_key: ActivityKey::new(),
            traces: vec![],
        };

        let (rust4pm_log, classifier) = value;

        for trace_index in 0..rust4pm_log.traces.len() {
            result.traces.push(
                rust4pm_log.traces[trace_index]
                    .events
                    .iter()
                    .map(|event| {
                        result
                            .activity_key
                            .process_activity(&classifier.get_class_identity(event))
                    })
                    .collect::<Vec<Activity>>(),
            );
        }

        result
    }
}
