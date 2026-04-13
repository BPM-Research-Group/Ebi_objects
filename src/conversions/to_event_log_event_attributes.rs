use crate::{
    AttributeKey, CompressedEventLogXes, EventLogCsv, EventLogOcel, EventLogXes, IntoTraceIterator,
    ebi_objects::{
        compressed_event_log_event_attributes::CompressedEventLogEventAttributes,
        event_log_event_attributes::EventLogEventAttributes,
    },
};
use ebi_activity_key::{Activity, ActivityKey};
use intmap::IntMap;
use process_mining::core::event_data::case_centric::{AttributeValue, EventLogClassifier};

impl From<CompressedEventLogEventAttributes> for EventLogEventAttributes {
    fn from(value: CompressedEventLogEventAttributes) -> Self {
        value.log
    }
}

impl From<CompressedEventLogXes> for EventLogEventAttributes {
    fn from(value: CompressedEventLogXes) -> Self {
        EventLogXes::into(value.log)
    }
}

impl From<EventLogXes> for EventLogEventAttributes {
    fn from(value: EventLogXes) -> Self {
        (value.rust4pm_log, value.classifier).into()
    }
}

impl From<(process_mining::EventLog, EventLogClassifier)> for EventLogEventAttributes {
    fn from(value: (process_mining::EventLog, EventLogClassifier)) -> Self {
        let (rust4pm_log, classifier) = value;
        let mut activity_key = ActivityKey::new();
        let mut attribute_key = AttributeKey::new();
        let traces = rust4pm_log
            .traces
            .into_iter()
            .map(|rust4pm_trace| {
                //convert trace
                let trace_activities = rust4pm_trace
                    .events
                    .iter()
                    .map(|event| {
                        activity_key.process_activity(&classifier.get_class_identity(event))
                    })
                    .collect::<Vec<Activity>>();

                //convert attributes
                let trace_attributes = rust4pm_trace
                    .events
                    .into_iter()
                    .map(|rust4pm_event| {
                        let mut attributes = IntMap::new();
                        for rust4pm_attribute in rust4pm_event.attributes {
                            let (attribute_name, attribute_value) =
                                (rust4pm_attribute.key, rust4pm_attribute.value);
                            let attribute = attribute_key
                                .process_attribute_value(&attribute_name, &attribute_value);
                            attributes.insert(attribute, attribute_value);
                        }
                        attributes
                    })
                    .collect::<Vec<_>>();

                (trace_activities, trace_attributes)
            })
            .collect();

        Self {
            traces,
            activity_key,
            attribute_key,
        }
    }
}

impl From<EventLogOcel> for EventLogEventAttributes {}

impl From<EventLogCsv> for EventLogEventAttributes {
    fn from(value: EventLogCsv) -> Self {
        let EventLogCsv {
            activity_attribute,
            activity_key,
            attribute_key,
            traces: csv_traces,
            ..
        } = value;

        let traces = csv_traces
            .into_iter()
            .map(|(_, csv_trace)| {
                //gather activity trace
                let activity_trace = csv_trace
                    .iter()
                    .map(|csv_attributes| {
                        //get the activity
                        let empty = String::new();
                        let activity_label = csv_attributes
                            .get(activity_attribute)
                            .unwrap_or_else(|| &empty);
                        activity_key
                            .process_activity_attempt(activity_label)
                            .unwrap()
                    })
                    .collect();

                //gather attributes trace
                let attributes_trace = csv_trace
                    .into_iter()
                    .map(|map| {
                        map.into_iter()
                            .map(|(attribute, csv_value)| {
                                (attribute, AttributeValue::String(csv_value))
                            })
                            .collect()
                    })
                    .collect();

                (activity_trace, attributes_trace)
            })
            .collect();
        Self {
            activity_key,
            attribute_key,
            traces,
        }
    }
}
