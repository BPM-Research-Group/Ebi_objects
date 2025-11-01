use intmap::IntMap;
use process_mining::event_log::event_log_struct::EventLogClassifier;

use crate::{
    Activity, ActivityKey, AttributeKey, CompressedEventLogXes, EventLogTraceAttributes,
    EventLogXes,
    ebi_objects::compressed_event_log_trace_attributes::CompressedEventLogTraceAttributes,
};

impl From<CompressedEventLogTraceAttributes> for EventLogTraceAttributes {
    fn from(value: CompressedEventLogTraceAttributes) -> Self {
        value.log
    }
}

impl From<CompressedEventLogXes> for EventLogTraceAttributes {
    fn from(value: CompressedEventLogXes) -> Self {
        EventLogXes::into(value.log)
    }
}

impl From<EventLogXes> for EventLogTraceAttributes {
    fn from(value: EventLogXes) -> Self {
        (value.rust4pm_log, value.classifier).into()
    }
}

impl From<(process_mining::EventLog, EventLogClassifier)> for EventLogTraceAttributes {
    fn from(value: (process_mining::EventLog, EventLogClassifier)) -> Self {
        let (rust4pm_log, classifier) = value;
        let mut activity_key = ActivityKey::new();
        let mut attribute_key = AttributeKey::new();
        let traces = rust4pm_log
            .traces
            .into_iter()
            .map(|rust4pm_trace| {
                //convert trace
                let trace = rust4pm_trace
                    .events
                    .iter()
                    .map(|event| {
                        activity_key.process_activity(&classifier.get_class_identity(event))
                    })
                    .collect::<Vec<Activity>>();

                //convert attributes
                let mut attributes = IntMap::new();
                for rust4pm_attribute in rust4pm_trace.attributes {
                    let (attribute_name, attribute_value) =
                        (rust4pm_attribute.key, rust4pm_attribute.value);
                    let attribute =
                        attribute_key.process_attribute_value(&attribute_name, &attribute_value);
                    attributes.insert(attribute, attribute_value);
                }

                (trace, attributes)
            })
            .collect();

        Self {
            traces,
            activity_key,
            attribute_key,
        }
    }
}
