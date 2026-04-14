use crate::{
    Attribute, AttributeKey, EbiObject, EventLogXes, Exportable, Importable, Infoable,
    IntoRefTraceIterator, NumberOfTraces,
    iterators::{
        event_attribute_iterator::{
            CategoricalEventAttributeIterator, NumericEventAttributeIterator,
            TimeEventAttributeIterator,
        },
        parallel_ref_trace_iterator::ParallelRefTraceIterator,
        ref_trace_iterator::RefTraceIterator,
    },
    log_infoable_startend, log_infoable_stats,
    traits::{
        attributes::Attributes,
        event_attribute_iterators::IntoEventAttributeIterator,
        event_attributes::EventAttributes,
        importable::{ImporterParameter, ImporterParameterValues, from_string},
        start_end_activities::StartEndActivities,
    },
};
use anyhow::{Error, Result, anyhow};
#[cfg(any(test, feature = "testactivities"))]
use ebi_activity_key::TestActivityKey;
use ebi_activity_key::{
    Activity, ActivityKey, ActivityKeyTranslator, HasActivityKey, TranslateActivityKey,
};
use ebi_arithmetic::{Fraction, One};
use ebi_derive::{ActivityKey, AttributeKey};
use intmap::IntMap;
use process_mining::core::event_data::case_centric::AttributeValue;
use std::{
    fmt,
    io::{BufRead, Write},
};

/// An event log with event attributes keeps track (and indexes) an event log in which each event is annotated with event attributes.
/// This is a separate struct, as the indexing and de-duplicating is not always necessary.
///
/// That is, please avoid this struct if all you need is activities.
#[derive(ActivityKey, AttributeKey, Clone)]
pub struct EventLogEventAttributes {
    pub activity_key: ActivityKey,
    pub attribute_key: AttributeKey,
    pub activity_attribute: Attribute,
    pub traces: Vec<(Vec<Activity>, Vec<IntMap<Attribute, AttributeValue>>)>,
}

impl EventLogEventAttributes {
    pub fn create_attribute_key(&mut self, rust4pm_log: process_mining::EventLog) {
        rust4pm_log.traces.iter().for_each(|trace| {
            trace.events.iter().for_each(|event| {
                for attribute in &event.attributes {
                    self.attribute_key
                        .process_attribute_value(&attribute.key, &attribute.value);
                }
            })
        })
    }
}

impl TranslateActivityKey for EventLogEventAttributes {
    fn translate_using_activity_key(&mut self, to_activity_key: &mut ActivityKey) {
        let translator = ActivityKeyTranslator::new(&self.activity_key, to_activity_key);
        self.traces.iter_mut().for_each(|trace| {
            trace
                .0
                .iter_mut()
                .for_each(|activity| translator.translate_activity_mut(activity))
        });
        self.activity_key = to_activity_key.clone();
    }
}

impl Importable for EventLogEventAttributes {
    const FILE_FORMAT_SPECIFICATION_LATEX: &str = EventLogXes::FILE_FORMAT_SPECIFICATION_LATEX;

    const IMPORTER_PARAMETERS: &[ImporterParameter] = EventLogXes::IMPORTER_PARAMETERS;

    fn import_as_object(
        reader: &mut dyn BufRead,
        parameter_values: &ImporterParameterValues,
    ) -> Result<EbiObject> {
        Ok(EbiObject::EventLogEventAttributes(Self::import(
            reader,
            parameter_values,
        )?))
    }

    fn import(reader: &mut dyn BufRead, parameter_values: &ImporterParameterValues) -> Result<Self>
    where
        Self: Sized,
    {
        let log = EventLogXes::import(reader, parameter_values)?;
        Ok(log.into())
    }
}
from_string!(EventLogEventAttributes);

impl Exportable for EventLogEventAttributes {
    fn export_from_object(object: EbiObject, f: &mut dyn Write) -> Result<()> {
        match object {
            EbiObject::EventLogTraceAttributes(log) => log.export(f),
            _ => Err(anyhow!("Cannot export as event log.")),
        }
    }

    fn export(&self, f: &mut dyn std::io::Write) -> Result<()> {
        writeln!(f, "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>")?;
        writeln!(f, "<log xes.version=\"1.0\">")?;
        writeln!(
            f,
            "<extension name=\"Concept\" prefix=\"concept\" uri=\"http://code.deckfour.org/xes/concept.xesext\"/>"
        )?;
        writeln!(
            f,
            "<global scope=\"event\"><string key=\"concept:name\" value=\"name\"/></global>"
        )?;
        writeln!(f, "<classifier name=\"Activity\" keys=\"concept:name\"/>")?;

        for (_, attributess) in self.traces.iter() {
            writeln!(f, "<trace>")?;

            //events
            for attributes in attributess {
                //attributes
                for (attribute, value) in attributes {
                    let attribute_label = self
                        .attribute_key
                        .attribute_to_label(attribute)
                        .ok_or_else(|| anyhow!("unknown attribute"))?;

                    let (tag_name, value_opt): (&str, Option<String>) = match value {
                        AttributeValue::String(s) => ("string", Some(s.to_string())),
                        AttributeValue::Date(d) => ("date", Some(d.to_rfc3339())),
                        AttributeValue::Int(i) => ("int", Some(i.to_string())),
                        AttributeValue::Float(f) => ("float", Some(f.to_string())),
                        AttributeValue::Boolean(b) => ("boolean", Some(b.to_string())),
                        AttributeValue::ID(id) => ("id", Some(id.to_string())),
                        AttributeValue::List(_) => ("list", None),
                        AttributeValue::Container(_) => ("container", None),
                        AttributeValue::None() => ("string", None),
                    };

                    if let Some(value) = value_opt {
                        writeln!(
                            f,
                            "<{} key=\"{}\" value=\"{}\"/>",
                            tag_name,
                            quick_xml::escape::escape(attribute_label),
                            quick_xml::escape::escape(value)
                        )?;
                    } else {
                        return Err(anyhow!(
                            "lists and containers are not supported by this exporter"
                        ));
                    }
                }
            }
            writeln!(f, "</trace>")?;
        }

        writeln!(f, "</log>")?;

        Ok(())
    }
}

impl fmt::Display for EventLogEventAttributes {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "event log with {} traces", self.number_of_traces())
    }
}

impl Infoable for EventLogEventAttributes {
    fn info(&self, f: &mut impl std::io::Write) -> Result<()> {
        let lengths = self.traces.iter().map(|t| t.0.len());
        log_infoable_stats!(f, self, lengths);

        writeln!(f, "")?;
        self.activity_key().info(f)?;

        log_infoable_startend!(f, self);

        writeln!(f, "")?;
        writeln!(f, "Event attributes:")?;
        self.attribute_key.info(f)?;

        Ok(writeln!(f, "")?)
    }
}

impl NumberOfTraces for EventLogEventAttributes {
    fn number_of_traces(&self) -> usize {
        self.traces.len()
    }

    fn number_of_events(&self) -> usize {
        self.traces.iter().map(|trace| trace.0.len()).sum()
    }
}

impl StartEndActivities for EventLogEventAttributes {
    fn start_activities(&self) -> IntMap<Activity, Fraction> {
        let mut result = IntMap::new();
        for (trace, _) in self.traces.iter() {
            if let Some(activity) = trace.iter().next() {
                match result.entry(*activity) {
                    intmap::Entry::Occupied(mut occupied_entry) => *occupied_entry.get_mut() += 1,
                    intmap::Entry::Vacant(vacant_entry) => {
                        vacant_entry.insert(Fraction::one());
                    }
                }
            }
        }
        result
    }

    fn end_activities(&self) -> IntMap<Activity, Fraction> {
        let mut result = IntMap::new();
        for (trace, _) in self.traces.iter() {
            if let Some(activity) = trace.iter().last() {
                match result.entry(*activity) {
                    intmap::Entry::Occupied(mut occupied_entry) => *occupied_entry.get_mut() += 1,
                    intmap::Entry::Vacant(vacant_entry) => {
                        vacant_entry.insert(Fraction::one());
                    }
                }
            }
        }
        result
    }
}

impl IntoRefTraceIterator for EventLogEventAttributes {
    fn iter_traces(&'_ self) -> RefTraceIterator<'_> {
        RefTraceIterator::VecTupleVecIntMap((&self.traces).into())
    }

    fn par_iter_traces(&self) -> ParallelRefTraceIterator<'_> {
        ParallelRefTraceIterator::VecTupleVecIntMap((&self.traces).into())
    }
}

impl IntoEventAttributeIterator for EventLogEventAttributes {
    fn iter_categorical_and_events(
        &self,
        attribute: Attribute,
    ) -> CategoricalEventAttributeIterator<'_> {
        (self, attribute).into()
    }

    fn iter_numeric_and_events(&self, attribute: Attribute) -> NumericEventAttributeIterator<'_> {
        (self, attribute).into()
    }

    fn iter_time_and_events(&self, attribute: Attribute) -> TimeEventAttributeIterator<'_> {
        (self, attribute).into()
    }
}

impl EventAttributes for EventLogEventAttributes {
    fn get_event_attribute_categorical(
        &self,
        trace_index: usize,
        event_index: usize,
        attribute: Attribute,
    ) -> Option<String> {
        self.traces
            .get(trace_index)?
            .1
            .get(event_index)?
            .get_attribute_categorical(attribute)
    }

    fn get_event_attribute_time(
        &self,
        trace_index: usize,
        event_index: usize,
        attribute: Attribute,
    ) -> Option<chrono::DateTime<chrono::FixedOffset>> {
        self.traces
            .get(trace_index)?
            .1
            .get(event_index)?
            .get_attribute_time(attribute)
    }

    fn get_event_attribute_numeric(
        &self,
        trace_index: usize,
        event_index: usize,
        attribute: Attribute,
    ) -> Option<ebi_arithmetic::Fraction> {
        self.traces
            .get(trace_index)?
            .1
            .get(event_index)?
            .get_attribute_numeric(attribute)
    }
}

#[cfg(any(test, feature = "testactivities"))]
impl TestActivityKey for EventLogEventAttributes {
    fn test_activity_key(&self) {
        self.traces.iter().for_each(|trace| {
            trace
                .0
                .iter()
                .for_each(|activity| self.activity_key().assert_activity_is_of_key(activity))
        });
    }
}
