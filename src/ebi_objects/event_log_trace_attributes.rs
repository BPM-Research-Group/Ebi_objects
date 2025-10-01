use anyhow::{Error, Result, anyhow};
use core::fmt;
use ebi_derive::ActivityKey;
use process_mining::{
    XESImportOptions,
    event_log::{Trace, event_log_struct::EventLogClassifier},
};
use std::{
    collections::HashMap,
    io::{self, BufRead, Write},
    str::FromStr,
};

use crate::{
    Activity, ActivityKey, ActivityKeyTranslator, Exportable, HasActivityKey, Importable,
    IndexTrace, Infoable, TranslateActivityKey, constants::ebi_object::EbiObject,
    data_type::DataType,
};

pub const FORMAT_SPECIFICATION: &str =
    "An event log file follows the IEEE XES format~\\cite{DBLP:journals/cim/AcamporaVSAGV17}. 
Parsing is performed by the Rust4PM crate~\\cite{DBLP:conf/bpm/KustersA24}.
For instance:
    \\lstinputlisting[language=xml, style=boxed]{../testfiles/a-b.xes}";

#[derive(ActivityKey, Clone)]
pub struct EventLogTraceAttributes {
    classifier: EventLogClassifier,
    pub activity_key: ActivityKey,
    pub traces: Vec<Vec<Activity>>,
    pub rust4pm_log: process_mining::EventLog,
}

impl EventLogTraceAttributes {
    pub fn new(log: process_mining::EventLog, classifier: EventLogClassifier) -> Self {
        let mut result = Self {
            classifier: classifier,
            rust4pm_log: log,
            activity_key: ActivityKey::new(),
            traces: vec![],
        };

        for trace_index in 0..result.rust4pm_log.traces.len() {
            result.traces.push(
                result.rust4pm_log.traces[trace_index]
                    .events
                    .iter()
                    .map(|event| {
                        result
                            .activity_key
                            .process_activity(&result.classifier.get_class_identity(event))
                    })
                    .collect::<Vec<Activity>>(),
            );
        }

        result
    }

    pub fn retain_traces_mut<F>(&mut self, f: &mut F)
    where
        F: FnMut((&Vec<Activity>, &Trace)) -> bool,
    {
        //get our hands free to change the traces without cloning
        let mut traces = vec![];
        let mut rust4pm_traces = vec![];
        std::mem::swap(&mut self.traces, &mut traces);
        std::mem::swap(&mut self.rust4pm_log.traces, &mut rust4pm_traces);

        (traces, rust4pm_traces) = traces
            .into_iter()
            .zip(rust4pm_traces.into_iter())
            .filter_map(|(mut trace, mut rust4pm_trace)| {
                if f((&mut trace, &mut rust4pm_trace)) {
                    Some((trace, rust4pm_trace))
                } else {
                    None
                }
            })
            .unzip();

        //swap the the traces back
        std::mem::swap(&mut self.traces, &mut traces);
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
        let translator = ActivityKeyTranslator::new(&self.activity_key, to_activity_key);
        self.traces
            .iter_mut()
            .for_each(|trace| translator.translate_trace_mut(trace));
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
        Ok(EventLogTraceAttributes::new(log, classifier))
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
            self.traces.iter().map(|trace| trace.len()).sum::<usize>()
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
        self.traces.len()
    }

    fn get_trace(&self, trace_index: usize) -> Option<&Vec<Activity>> {
        self.traces.get(trace_index)
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
        assert_eq!(log.rust4pm_log.traces.len(), 2);

        log.retain_traces_mut(&mut |_| false);

        assert_eq!(log.number_of_traces(), 0);
        assert_eq!(log.rust4pm_log.traces.len(), 0);
    }
}
