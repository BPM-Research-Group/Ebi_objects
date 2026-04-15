use std::collections::{HashMap, HashSet};

use crate::{
    AttributeKey, CompressedEventLogXes, EventLogCsv, EventLogOcel, EventLogPython, EventLogXes, ebi_objects::{
        compressed_event_log_event_attributes::CompressedEventLogEventAttributes,
        event_log_event_attributes::EventLogEventAttributes,
    }
};
use ebi_activity_key::{Activity, ActivityKey};
use intmap::IntMap;
use process_mining::{
    OCEL,
    core::event_data::case_centric::{AttributeValue, EventLogClassifier},
};

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

impl From<EventLogPython> for EventLogEventAttributes {
    fn from(value: EventLogPython) -> Self {
        value.log.into()
    }
}

impl From<EventLogXes> for EventLogEventAttributes {
    fn from(value: EventLogXes) -> Self {
        (
            value.rust4pm_log,
            value.classifier,
            value.resource_attribute,
            value.time_attribute,
        )
            .into()
    }
}

impl From<(process_mining::EventLog, EventLogClassifier, String, String)>
    for EventLogEventAttributes
{
    fn from(value: (process_mining::EventLog, EventLogClassifier, String, String)) -> Self {
        let (rust4pm_log, classifier, resource, time) = value;
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

        let activity_attribute = attribute_key
            .label_to_attribute(classifier.keys.iter().next().unwrap_or(&String::new()))
            .unwrap_or(attribute_key.id_to_attribute(0));

        let time_attribute = attribute_key.label_to_attribute(&time);
        let resource_attribute = attribute_key.label_to_attribute(&resource);

        Self {
            traces,
            activity_key,
            activity_attribute,
            attribute_key,
            time_attribute,
            resource_attribute,
        }
    }
}

impl From<EventLogOcel> for EventLogEventAttributes {
    fn from(value: EventLogOcel) -> Self {
        log::info!("Convert OCEL event log into event log.");
        let EventLogOcel {
            mut activity_key,
            rust4pm_log,
            case_object_type,
            resource_object_type,
            time_attribute,
        } = value;
        let OCEL {
            event_types: _,
            object_types: _,
            events,
            objects,
        } = rust4pm_log;

        //gather attributes
        let mut attribute_key = AttributeKey::new();
        let case_object_id_attribute = attribute_key.process_attribute_column(0, "a");
        let activity_attribute = attribute_key.process_attribute_column(1, "a");
        let resource_attribute = attribute_key.process_attribute_column(2, "a");

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

        //gather list of objects that form the cases
        let case_objects = objects
            .iter()
            .filter_map(|ob| {
                if ob.object_type == case_object_type {
                    Some(ob.id.clone())
                } else {
                    None
                }
            })
            .collect::<HashSet<_>>();

        let resource_objects = objects
            .into_iter()
            .filter_map(|ob| {
                if let Some(resource_object_type) = &resource_object_type
                    && ob.object_type == *resource_object_type
                {
                    Some(ob.id)
                } else {
                    None
                }
            })
            .collect::<HashSet<_>>();

        //gather traces and attributes
        let mut object_id2trace_activities = HashMap::new();
        let mut object_id2trace_attributes = HashMap::new();
        for event in events {
            for relation in &event.relationships {
                if case_objects.contains(&relation.object_id) {
                    //activity trace
                    object_id2trace_activities
                        .entry(relation.object_id.clone())
                        .or_insert_with(|| vec![])
                        .push(activity_key.process_activity(&event.event_type));

                    //attribute trace
                    let mut attributes = IntMap::new();
                    for rust4pm_attribute in &event.attributes {
                        let attribute_value = AttributeKey::ocel_attribute_value2attribute_value(
                            rust4pm_attribute.value.clone(),
                        );
                        let attribute = attribute_key
                            .process_attribute_value(&rust4pm_attribute.name, &attribute_value);

                        attributes.insert(attribute, attribute_value);
                    }
                    attributes.insert_checked(
                        case_object_id_attribute,
                        AttributeValue::String(relation.object_id.clone()),
                    );

                    //resource
                    if resource_object_type.is_some() {
                        for relation2 in &event.relationships {
                            if resource_objects.contains(&relation2.object_id) {
                                attributes.insert(
                                    resource_attribute,
                                    AttributeValue::String(relation2.object_id.clone()),
                                );
                            }
                        }
                    }

                    object_id2trace_attributes
                        .entry(relation.object_id.clone())
                        .or_insert_with(|| vec![])
                        .push(attributes);
                }
            }
        }

        //merge traces with attributes
        let mut traces = vec![];
        for (object_id, _) in object_id2attributes {
            //no events -> empty trace
            if !object_id2trace_activities.contains_key(&object_id) {
                traces.push((vec![], vec![]));
            }
        }
        for (object_id, trace) in object_id2trace_activities {
            let attributes = object_id2trace_attributes.get(&object_id).unwrap();
            traces.push((trace, attributes.clone()));
        }

        //obtain the time attribute, if it was set by any event
        let time_attribute = attribute_key.label_to_attribute(&time_attribute);

        //construct result
        Self {
            activity_key,
            attribute_key,
            traces,
            activity_attribute,
            resource_attribute: if resource_object_type.is_some() {
                Some(resource_attribute)
            } else {
                None
            },
            time_attribute,
        }
    }
}

impl From<EventLogCsv> for EventLogEventAttributes {
    fn from(value: EventLogCsv) -> Self {
        let EventLogCsv {
            activity_attribute,
            activity_key,
            attribute_key,
            traces: csv_traces,
            resource_attribute,
            time_attribute,
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
            activity_attribute,
            resource_attribute,
            time_attribute,
            traces,
        }
    }
}
