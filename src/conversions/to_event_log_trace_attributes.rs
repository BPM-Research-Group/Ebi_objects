use crate::{
    Activity, ActivityKey, AttributeKey, CompressedEventLogXes, EventLogTraceAttributes,
    EventLogXes,
    ebi_objects::{
        compressed_event_log_trace_attributes::CompressedEventLogTraceAttributes,
        event_log_ocel::EventLogOcel,
    },
};
use intmap::IntMap;
use process_mining::{
    OCEL,
    core::event_data::case_centric::{AttributeValue, EventLogClassifier},
};
use std::collections::HashMap;

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

impl From<EventLogOcel> for EventLogTraceAttributes {
    fn from(value: EventLogOcel) -> Self {
        log::info!("Convert OCEL event log into event log.");
        let EventLogOcel {
            mut activity_key,
            rust4pm_log,
            case_object_type: object_type,
            ..
        } = value;
        let OCEL {
            event_types: _,
            object_types: _,
            events,
            objects,
        } = rust4pm_log;

        //gather attributes
        let mut attribute_key = AttributeKey::new();
        let mut object_id2attributes = HashMap::new();
        for object in objects.iter() {
            let mut attributes = IntMap::new();
            for rust4pm_attribute in &object.attributes {
                let attribute_value = AttributeKey::ocel_attribute_value2attribute_value(
                    rust4pm_attribute.value.clone(),
                );
                let attribute = attribute_key
                    .process_attribute_value(&rust4pm_attribute.name, &attribute_value);

                attributes.insert(attribute, attribute_value);
            }
            object_id2attributes.insert(object.id.clone(), attributes);
        }
        let attribute_object_id = "object-id";

        //gather list of objects and create empty traces
        let mut object_id2trace = EventLogOcel::get_relevant_objects(&objects, &object_type)
            .into_iter()
            .map(|object_id| (object_id, vec![]))
            .collect::<HashMap<_, _>>();

        //gather traces
        for event in events {
            for relation in event.relationships {
                if let Some(trace) = object_id2trace.get_mut(&relation.object_id) {
                    trace.push(activity_key.process_activity(&event.event_type));
                }
            }
        }

        //merge traces with attributes
        let mut traces = vec![];
        for (object_id, trace) in object_id2trace {
            traces.push((
                trace,
                if let Some(mut attributes) = object_id2attributes.remove(&object_id) {
                    let attribute_object_id = attribute_key.process_attribute_value(
                        attribute_object_id,
                        &AttributeValue::String(object_id.clone()),
                    );
                    attributes
                        .insert_checked(attribute_object_id, AttributeValue::String(object_id));
                    attributes
                } else {
                    //no attributes
                    let attribute_object_id = attribute_key.process_attribute_value(
                        attribute_object_id,
                        &AttributeValue::String(object_id.clone()),
                    );
                    let mut attributes = IntMap::new();
                    attributes
                        .insert_checked(attribute_object_id, AttributeValue::String(object_id));
                    attributes
                },
            ));
        }
        for (object_id, mut attributes) in object_id2attributes {
            //no events -> empty trace
            let attribute_object_id = attribute_key.process_attribute_value(
                attribute_object_id,
                &AttributeValue::String(object_id.clone()),
            );
            attributes.insert_checked(attribute_object_id, AttributeValue::String(object_id));
            traces.push((vec![], attributes));
        }

        //construct result
        Self {
            activity_key,
            attribute_key,
            traces,
        }
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
