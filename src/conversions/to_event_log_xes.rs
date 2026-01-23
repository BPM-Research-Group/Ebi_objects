use crate::{
    ActivityKey, CompressedEventLogXes, EventLogCsv,
    ebi_objects::{event_log_python::EventLogPython, event_log_xes::EventLogXes},
};
use anyhow::{Error, anyhow};
use process_mining::core::event_data::case_centric::{
    Attribute, AttributeValue, Event, EventLogClassifier, Trace,
};

impl From<CompressedEventLogXes> for EventLogXes {
    fn from(value: CompressedEventLogXes) -> Self {
        value.log
    }
}

impl TryFrom<EventLogCsv> for EventLogXes {
    type Error = Error;

    fn try_from(value: EventLogCsv) -> Result<Self, Self::Error> {
        let mut result = process_mining::EventLog::new();

        //traces
        for (trace_id, trace) in value.traces {
            let mut new_trace = Trace::new();

            //set concept:name of trace
            new_trace.attributes.push(Attribute::new(
                "concept:name".to_string(),
                AttributeValue::String(trace_id),
            ));

            //add events
            for event in trace {
                let mut new_event = Event {
                    attributes: vec![].into(),
                };

                for (attribute_key, attribute_value) in event {
                    let attribute_name = value
                        .attribute_key
                        .attribute_to_label(attribute_key)
                        .ok_or_else(|| anyhow!("could not find attribute in attribute map"))?;

                    new_event.attributes.push(Attribute::new(
                        attribute_name.to_string(),
                        AttributeValue::String(attribute_value),
                    ));
                }

                new_trace.events.push(new_event);
            }

            result.traces.push(new_trace);
        }

        //construct classifier
        let activity_attribute_string = value
            .attribute_key
            .attribute_to_label(value.activity_attribute)
            .ok_or_else(|| anyhow!("could not retrieve activity attribute"))?
            .to_string();
        let classifier = EventLogClassifier {
            name: activity_attribute_string.clone(),
            keys: vec![activity_attribute_string],
        };

        Ok((result, classifier).into())
    }
}

impl From<(process_mining::EventLog, EventLogClassifier)> for EventLogXes {
    fn from(value: (process_mining::EventLog, EventLogClassifier)) -> Self {
        let mut result = Self {
            classifier: value.1,
            rust4pm_log: value.0,
            activity_key: ActivityKey::new(),
        };
        result.create_activity_key();
        result
    }
}

impl From<EventLogPython> for EventLogXes {
    fn from(value: EventLogPython) -> Self {
        value.log
    }
}
