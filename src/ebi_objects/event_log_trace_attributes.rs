use crate::{
    Activity, ActivityKey, ActivityKeyTranslator, Attribute, AttributeKey, EventLogXes, Exportable,
    HasActivityKey, Importable, Infoable, IntoAttributeIterator, IntoAttributeTraceIterator,
    IntoRefTraceIterator, TranslateActivityKey,
    constants::ebi_object::EbiObject,
    iterators::{
        attribute_iterator::{
            CategoricalAttributeIterator, NumericAttributeIterator, TimeAttributeIterator,
        },
        parallel_ref_trace_iterator::ParallelRefTraceIterator,
        ref_trace_iterator::RefTraceIterator,
    },
    traits::{
        importable::{ImporterParameter, ImporterParameterValues, from_string},
        number_of_traces::NumberOfTraces,
        start_end_activities::StartEndActivities,
        trace_attributes::TraceAttributes,
    },
};
use anyhow::{Result, anyhow};
use chrono::{DateTime, FixedOffset};
use ebi_arithmetic::{Fraction, One};
use ebi_derive::{ActivityKey, AttributeKey};
use intmap::IntMap;
use process_mining::event_log::AttributeValue;
use std::{
    fmt,
    io::{BufRead, Write},
};

#[derive(ActivityKey, AttributeKey, Clone)]
pub struct EventLogTraceAttributes {
    pub activity_key: ActivityKey,
    pub attribute_key: AttributeKey,
    pub traces: Vec<(Vec<Activity>, IntMap<Attribute, AttributeValue>)>,
}

impl EventLogTraceAttributes {
    pub fn create_attribute_key(&mut self, rust4pm_log: process_mining::EventLog) {
        rust4pm_log.traces.iter().for_each(|trace| {
            for attribute in &trace.attributes {
                self.attribute_key
                    .process_attribute_value(&attribute.key, &attribute.value);
            }
        })
    }

    pub fn retain_traces_mut<F>(&mut self, f: &mut F)
    where
        F: FnMut(&Vec<Activity>) -> bool,
    {
        //get our hands free to change the traces without cloning
        let mut traces = vec![];
        std::mem::swap(&mut self.traces, &mut traces);

        traces = traces
            .into_iter()
            .filter_map(|(mut trace, atts)| {
                if f(&mut trace) {
                    Some((trace, atts))
                } else {
                    None
                }
            })
            .collect();

        //swap the the traces back
        std::mem::swap(&mut self.traces, &mut traces);
    }

    pub fn retain_traces_attributes_mut<F>(&mut self, f: &mut F)
    where
        F: FnMut(&(Vec<Activity>, IntMap<Attribute, AttributeValue>)) -> bool,
    {
        //get our hands free to change the traces without cloning
        let mut traces = vec![];
        std::mem::swap(&mut self.traces, &mut traces);

        traces = traces
            .into_iter()
            .filter_map(|mut trace| if f(&mut trace) { Some(trace) } else { None })
            .collect();

        //swap the the traces back
        std::mem::swap(&mut self.traces, &mut traces);
    }
}

impl TranslateActivityKey for EventLogTraceAttributes {
    fn translate_using_activity_key(&mut self, to_activity_key: &mut ActivityKey) {
        let translator = ActivityKeyTranslator::new(&self.activity_key, to_activity_key);
        self.traces
            .iter_mut()
            .for_each(|(trace, _)| translator.translate_trace_mut(trace));
        self.activity_key = to_activity_key.clone();
    }
}

impl Importable for EventLogTraceAttributes {
    const FILE_FORMAT_SPECIFICATION_LATEX: &str = EventLogXes::FILE_FORMAT_SPECIFICATION_LATEX;

    const IMPORTER_PARAMETERS: &[ImporterParameter] = EventLogXes::IMPORTER_PARAMETERS;

    fn import_as_object(
        reader: &mut dyn BufRead,
        parameter_values: &ImporterParameterValues,
    ) -> Result<EbiObject> {
        Ok(EbiObject::EventLogTraceAttributes(Self::import(
            reader,
            parameter_values,
        )?))
    }

    fn import(
        reader: &mut dyn BufRead,
        parameter_values: &ImporterParameterValues,
    ) -> anyhow::Result<Self>
    where
        Self: Sized,
    {
        let log = EventLogXes::import(reader, parameter_values)?;
        Ok(log.into())
    }
}
from_string!(EventLogTraceAttributes);

impl Exportable for EventLogTraceAttributes {
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
            "<global scope=\"trace\"><string key=\"concept:name\" value=\"name\"/></global>"
        )?;
        writeln!(
            f,
            "<global scope=\"event\"><string key=\"concept:name\" value=\"name\"/></global>"
        )?;
        writeln!(f, "<classifier name=\"Activity\" keys=\"concept:name\"/>")?;

        for (trace, attributes) in self.traces.iter() {
            writeln!(f, "<trace>")?;

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

            //events
            for activity in trace {
                writeln!(
                    f,
                    "<event><string key=\"concept:name\" value=\"{}\"/></event>",
                    quick_xml::escape::escape(self.activity_key().get_activity_label(activity))
                )?;
            }
            writeln!(f, "</trace>")?;
        }

        writeln!(f, "</log>")?;

        Ok(())
    }
}

impl fmt::Display for EventLogTraceAttributes {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "event log with {} traces", self.number_of_traces())
    }
}

impl Infoable for EventLogTraceAttributes {
    fn info(&self, f: &mut impl std::io::Write) -> Result<()> {
        writeln!(f, "Number of traces\t{}", self.number_of_traces())?;
        writeln!(f, "Number of events\t{}", self.number_of_events())?;
        writeln!(
            f,
            "Number of activities\t{}",
            self.activity_key().get_number_of_activities()
        )?;

        writeln!(f, "")?;
        self.activity_key().info(f)?;

        writeln!(f, "")?;
        writeln!(f, "Start activities")?;
        for (activity, cardinality) in self.start_activites() {
            writeln!(
                f,
                " {}: {}",
                self.activity_key.get_activity_label(&activity),
                cardinality
            )?;
        }

        writeln!(f, "")?;
        writeln!(f, "End activities")?;
        for (activity, cardinality) in self.end_activites() {
            writeln!(
                f,
                " {}: {}",
                self.activity_key.get_activity_label(&activity),
                cardinality
            )?;
        }

        writeln!(f, "")?;
        writeln!(f, "Trace attributes:")?;
        self.attribute_key.info(f)?;

        Ok(writeln!(f, "")?)
    }
}

impl NumberOfTraces for EventLogTraceAttributes {
    fn number_of_traces(&self) -> usize {
        self.traces.len()
    }

    fn number_of_events(&self) -> usize {
        self.traces.iter().map(|(trace, _)| trace.len()).sum()
    }
}

impl StartEndActivities for EventLogTraceAttributes {
    fn start_activites(&self) -> IntMap<Activity, Fraction> {
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

    fn end_activites(&self) -> IntMap<Activity, Fraction> {
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

impl IntoRefTraceIterator for EventLogTraceAttributes {
    fn iter_traces(&'_ self) -> RefTraceIterator<'_> {
        RefTraceIterator::VecTupleIntMap((&self.traces).into())
    }

    fn par_iter_traces(&self) -> ParallelRefTraceIterator<'_> {
        ParallelRefTraceIterator::VecTupleIntMap((&self.traces).into())
    }
}

impl IntoAttributeTraceIterator for EventLogTraceAttributes {
    fn iter_categorical_and_traces(
        &self,
        attribute: Attribute,
    ) -> std::iter::Zip<RefTraceIterator<'_>, CategoricalAttributeIterator<'_>> {
        let x: RefTraceIterator = RefTraceIterator::VecTupleIntMap((&self.traces).into());
        let y: CategoricalAttributeIterator = (self, attribute).into();
        x.zip(y)
    }

    fn iter_numeric_and_traces(
        &self,
        attribute: Attribute,
    ) -> std::iter::Zip<RefTraceIterator<'_>, NumericAttributeIterator<'_>> {
        let x: RefTraceIterator = RefTraceIterator::VecTupleIntMap((&self.traces).into());
        let y: NumericAttributeIterator = (self, attribute).into();
        x.zip(y)
    }

    fn iter_time_and_traces(
        &self,
        attribute: Attribute,
    ) -> std::iter::Zip<RefTraceIterator<'_>, TimeAttributeIterator<'_>> {
        let x: RefTraceIterator = RefTraceIterator::VecTupleIntMap((&self.traces).into());
        let y: TimeAttributeIterator = (self, attribute).into();
        x.zip(y)
    }
}

impl IntoAttributeIterator for EventLogTraceAttributes {
    fn iter_categorical(&self, attribute: Attribute) -> CategoricalAttributeIterator<'_> {
        (self, attribute).into()
    }

    fn iter_numeric(&self, attribute: Attribute) -> NumericAttributeIterator<'_> {
        (self, attribute).into()
    }

    fn iter_time(&self, attribute: Attribute) -> TimeAttributeIterator<'_> {
        (self, attribute).into()
    }
}

impl TraceAttributes for EventLogTraceAttributes {
    fn get_trace_attribute_categorical(
        &self,
        trace_index: usize,
        attribute: Attribute,
    ) -> Option<String> {
        match self.traces.get(trace_index)?.1.get(attribute)? {
            AttributeValue::String(x) => Some(x.to_owned()),
            AttributeValue::Date(_) => None,
            AttributeValue::Int(x) => Some(x.to_string()),
            AttributeValue::Float(x) => Some(x.to_string()),
            AttributeValue::Boolean(x) => Some(x.to_string()),
            AttributeValue::ID(_) => None,
            AttributeValue::List(_) => None,
            AttributeValue::Container(_) => None,
            AttributeValue::None() => None,
        }
    }

    fn get_trace_attribute_time(
        &self,
        trace_index: usize,
        attribute: Attribute,
    ) -> Option<chrono::DateTime<chrono::FixedOffset>> {
        match self.traces.get(trace_index)?.1.get(attribute)? {
            AttributeValue::String(x) => Some(x.parse::<DateTime<FixedOffset>>().ok()?),
            AttributeValue::Date(x) => Some(*x),
            AttributeValue::Int(_) => None,
            AttributeValue::Float(_) => None,
            AttributeValue::Boolean(_) => None,
            AttributeValue::ID(_) => None,
            AttributeValue::List(_) => None,
            AttributeValue::Container(_) => None,
            AttributeValue::None() => None,
        }
    }

    fn get_trace_attribute_numeric(
        &self,
        trace_index: usize,
        attribute: Attribute,
    ) -> Option<ebi_arithmetic::Fraction> {
        match self.traces.get(trace_index)?.1.get(attribute)? {
            AttributeValue::String(x) => Some(x.parse::<Fraction>().ok()?),
            AttributeValue::Date(_) => None,
            AttributeValue::Int(x) => Some(Fraction::from(*x)),
            AttributeValue::Float(x) => Some(x.to_string().parse::<Fraction>().ok()?),
            AttributeValue::Boolean(_) => None,
            AttributeValue::ID(_) => None,
            AttributeValue::List(_) => None,
            AttributeValue::Container(_) => None,
            AttributeValue::None() => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use std::fs;

    use crate::{
        ActivityKey, NumberOfTraces, TranslateActivityKey,
        ebi_objects::finite_stochastic_language::FiniteStochasticLanguage,
    };

    use super::EventLogTraceAttributes;

    #[test]
    fn log_to_slang() {
        let fin = fs::read_to_string("testfiles/a-b.xes").unwrap();
        let log = fin.parse::<EventLogTraceAttributes>().unwrap();

        let fin1 = fs::read_to_string("testfiles/a-b.slang").unwrap();
        let slang = fin1.parse::<FiniteStochasticLanguage>().unwrap();

        assert_eq!(Into::<FiniteStochasticLanguage>::into(log), slang);
    }

    #[test]
    fn log_activity_key() {
        let fin = fs::read_to_string("testfiles/a-b.xes").unwrap();
        let mut log = fin.parse::<EventLogTraceAttributes>().unwrap();

        let mut activity_key = ActivityKey::new();
        log.translate_using_activity_key(&mut activity_key);
    }

    #[test]
    fn log_display() {
        let fin = fs::read_to_string("testfiles/a-b.xes").unwrap();
        let log = fin.parse::<EventLogTraceAttributes>().unwrap();

        assert_eq!(format!("{}", log), "event log with 2 traces");
    }

    #[test]
    fn len_retain_mut() {
        let fin = fs::read_to_string("testfiles/a-b.xes").unwrap();
        let mut log = fin.parse::<EventLogTraceAttributes>().unwrap();

        assert_eq!(log.number_of_traces(), 2);

        log.retain_traces_attributes_mut(&mut |_| false);

        assert_eq!(log.number_of_traces(), 0);
    }
}
