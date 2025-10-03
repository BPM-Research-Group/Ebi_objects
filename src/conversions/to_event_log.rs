use process_mining::event_log::event_log_struct::EventLogClassifier;

use crate::{
    Activity, ActivityKey, EventLogTraceAttributes, IndexTrace,
    ebi_objects::{compressed_event_log::CompressedEventLog, event_log::EventLog},
};

impl From<CompressedEventLog> for EventLog {
    fn from(value: CompressedEventLog) -> Self {
        log::info!("Convert compressed event log into event log.");
        value.log
    }
}

impl From<EventLogTraceAttributes> for EventLog {
    fn from(mut value: EventLogTraceAttributes) -> Self {
        let mut traces = vec![];

        for trace_index in 0..value.number_of_traces() {
            traces.push(
                value.rust4pm_log.traces[trace_index]
                    .events
                    .iter()
                    .map(|event| {
                        value
                            .activity_key
                            .process_activity(&value.classifier.get_class_identity(event))
                    })
                    .collect::<Vec<Activity>>(),
            );
        }
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
