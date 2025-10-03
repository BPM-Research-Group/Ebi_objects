use crate::{
    Activity, ActivityKey, Attribute, AttributeKey, Exportable, HasActivityKey, Importable,
    IndexTrace, Infoable, TranslateActivityKey, constants::ebi_object::EbiObject,
    data_type::DataType, traits::index_trace_attributes::IndexTraceAttributes,
};
use anyhow::{Error, Result, anyhow};
use chrono::{DateTime, FixedOffset};
use core::fmt;
use ebi_arithmetic::Fraction;
use ebi_derive::ActivityKey;
use process_mining::{
    XESImportOptions,
    event_log::{
        AttributeValue, Trace, XESEditableAttribute, event_log_struct::EventLogClassifier,
    },
};
use std::{
    collections::HashMap,
    io::{self, BufRead, Write},
    mem,
    str::FromStr,
};

pub const FORMAT_SPECIFICATION: &str =
    "An event log file follows the IEEE XES format~\\cite{DBLP:journals/cim/AcamporaVSAGV17}. 
Parsing is performed by the Rust4PM crate~\\cite{DBLP:conf/bpm/KustersA24}.
For instance:
    \\lstinputlisting[language=xml, style=boxed]{../testfiles/a-b.xes}";

#[derive(ActivityKey, Clone)]
pub struct EventLogTraceAttributes {
    pub classifier: EventLogClassifier,
    pub activity_key: ActivityKey,
    pub rust4pm_log: process_mining::EventLog,
    pub attribute_key: AttributeKey,
}

impl EventLogTraceAttributes {
    pub fn process_activity_key(&mut self) {
        let mut activity_key = ActivityKey::new();
        mem::swap(&mut activity_key, &mut self.activity_key);
        self.rust4pm_log.traces.iter().for_each(|trace| {
            trace.events.iter().for_each(|event| {
                activity_key.process_activity(&self.classifier.get_class_identity(event));
            })
        });
        mem::swap(&mut activity_key, &mut self.activity_key);
    }

    pub fn retain_traces_mut<F>(&mut self, f: &mut F)
    where
        F: FnMut(&Trace) -> bool,
    {
        //get our hands free to change the traces without cloning
        let mut rust4pm_traces = vec![];
        std::mem::swap(&mut self.rust4pm_log.traces, &mut rust4pm_traces);

        rust4pm_traces = rust4pm_traces
            .into_iter()
            .filter_map(|mut rust4pm_trace| {
                if f(&mut rust4pm_trace) {
                    Some(rust4pm_trace)
                } else {
                    None
                }
            })
            .collect();

        //swap the the traces back
        std::mem::swap(&mut self.rust4pm_log.traces, &mut rust4pm_traces);
    }

    pub fn get_trace_attributes(&self) -> HashMap<String, DataType> {
        let mut map: HashMap<String, DataType> = HashMap::new();
        for trace in &self.rust4pm_log.traces {
            for attribute in &trace.attributes {
                match map.entry(attribute.key.clone()) {
                    std::collections::hash_map::Entry::Occupied(mut e) => {
                        e.get_mut().update(&attribute.value);
                        ()
                    }
                    std::collections::hash_map::Entry::Vacant(e) => {
                        e.insert(DataType::init(&attribute.value));
                        ()
                    }
                }
            }
        }
        map
    }
}

impl TranslateActivityKey for EventLogTraceAttributes {
    fn translate_using_activity_key(&mut self, to_activity_key: &mut ActivityKey) {
        self.activity_key = to_activity_key.clone();
    }
}

impl Importable for EventLogTraceAttributes {
    fn import_as_object(reader: &mut dyn BufRead) -> Result<EbiObject> {
        Ok(EbiObject::EventLogTraceAttributes(Self::import(reader)?))
    }

    fn import(reader: &mut dyn BufRead) -> anyhow::Result<Self>
    where
        Self: Sized,
    {
        let log =
            process_mining::event_log::import_xes::import_xes(reader, XESImportOptions::default());
        if log.is_err() {
            return Err(anyhow!("{}", log.err().unwrap()));
        }
        let log = log.unwrap();
        let classifier = EventLogClassifier {
            name: "concept:name".to_string(),
            keys: vec!["concept:name".to_string()],
        };
        Ok(EventLogTraceAttributes::from((log, classifier)))
    }
}

impl FromStr for EventLogTraceAttributes {
    type Err = Error;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        let mut reader = io::Cursor::new(s);
        Self::import(&mut reader)
    }
}

impl Exportable for EventLogTraceAttributes {
    fn export_from_object(object: EbiObject, f: &mut dyn Write) -> Result<()> {
        match object {
            EbiObject::EventLog(log) => log.export(f),
            _ => Err(anyhow!("Cannot export as event log.")),
        }
    }

    fn export(&self, f: &mut dyn std::io::Write) -> Result<()> {
        process_mining::event_log::export_xes::export_xes_event_log(f, &self.rust4pm_log)?;
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
        writeln!(
            f,
            "Number of events\t{}",
            self.rust4pm_log
                .traces
                .iter()
                .map(|trace| trace.events.len())
                .sum::<usize>()
        )?;
        writeln!(
            f,
            "Number of activities\t{}",
            self.activity_key().get_number_of_activities()
        )?;

        writeln!(f, "")?;
        self.activity_key().info(f)?;

        let trace_atts = self.get_trace_attributes();
        let t: Vec<String> = trace_atts
            .iter()
            .map(|(att, data_type)| format!("{}\t{}", att, data_type))
            .collect();
        writeln!(f, "Trace attributes:")?;
        writeln!(f, "\t{}", t.join("\n\t"))?;

        Ok(write!(f, "")?)
    }
}

impl IndexTrace for EventLogTraceAttributes {
    fn number_of_traces(&self) -> usize {
        self.rust4pm_log.traces.len()
    }

    fn get_trace<'a>(
        &self,
        trace_index: usize,
        result_cache: &'a mut Vec<Activity>,
    ) -> Option<&'a Vec<Activity>> {
        result_cache.clear();

        for event in self.rust4pm_log.traces.get(trace_index)?.events.iter() {
            let activity = self
                .activity_key
                .process_activity_attempt(&self.classifier.get_class_identity(event))?;
            result_cache.push(activity);
        }

        Some(result_cache)
    }
}

impl IndexTraceAttributes for EventLogTraceAttributes {
    fn get_trace_attribute_categorical(
        &self,
        trace_index: usize,
        attribute: Attribute,
    ) -> Option<String> {
        let attribute = self
            .rust4pm_log
            .traces
            .get(trace_index)?
            .attributes
            .get_by_key(self.attribute_key.attribute_to_label(attribute)?)?;

        match &attribute.value {
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
        let attribute = self
            .rust4pm_log
            .traces
            .get(trace_index)?
            .attributes
            .get_by_key(self.attribute_key.attribute_to_label(attribute)?)?;

        match &attribute.value {
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
        let attribute = self
            .rust4pm_log
            .traces
            .get(trace_index)?
            .attributes
            .get_by_key(self.attribute_key.attribute_to_label(attribute)?)?;

        match &attribute.value {
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
        ActivityKey, IndexTrace, TranslateActivityKey,
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

        log.retain_traces_mut(&mut |_| false);

        assert_eq!(log.number_of_traces(), 0);
    }
}
