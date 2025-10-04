use anyhow::{Error, Result, anyhow};
use core::fmt;
use ebi_derive::ActivityKey;
use process_mining::{XESImportOptions, event_log::event_log_struct::EventLogClassifier};
use std::{
    io::{self, BufRead, Write},
    slice::Iter,
    str::FromStr,
};

use crate::{
    Activity, ActivityKey, ActivityKeyTranslator, Exportable, HasActivityKey, Importable,
    IndexTrace, Infoable, TranslateActivityKey, constants::ebi_object::EbiObject,
};

pub const FORMAT_SPECIFICATION: &str =
    "An event log file follows the IEEE XES format~\\cite{DBLP:journals/cim/AcamporaVSAGV17}. 
Parsing is performed by the Rust4PM crate~\\cite{DBLP:conf/bpm/KustersA24}.
For instance:
    \\lstinputlisting[language=xml, style=boxed]{../testfiles/a-b.xes}";

#[derive(ActivityKey, Clone)]
pub struct EventLog {
    pub activity_key: ActivityKey,
    pub traces: Vec<Vec<Activity>>,
}

impl EventLog {
    pub fn retain_traces_mut<F>(&mut self, f: &mut F)
    where
        F: FnMut(&Vec<Activity>) -> bool,
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

impl TranslateActivityKey for EventLog {
    fn translate_using_activity_key(&mut self, to_activity_key: &mut ActivityKey) {
        let translator = ActivityKeyTranslator::new(&self.activity_key, to_activity_key);
        self.traces
            .iter_mut()
            .for_each(|trace| translator.translate_trace_mut(trace));
        self.activity_key = to_activity_key.clone();
    }
}

impl Importable for EventLog {
    fn import_as_object(reader: &mut dyn BufRead) -> Result<EbiObject> {
        Ok(EbiObject::EventLog(Self::import(reader)?))
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
        Ok((log, classifier).into())
    }
}

impl FromStr for EventLog {
    type Err = Error;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        let mut reader = io::Cursor::new(s);
        Self::import(&mut reader)
    }
}

impl Exportable for EventLog {
    fn export_from_object(object: EbiObject, f: &mut dyn Write) -> Result<()> {
        match object {
            EbiObject::EventLog(log) => log.export(f),
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

        for trace in self.traces.iter() {
            writeln!(f, "<trace>")?;
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

impl fmt::Display for EventLog {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "event log with {} traces", self.number_of_traces())
    }
}

impl Infoable for EventLog {
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

        Ok(write!(f, "")?)
    }
}

impl IndexTrace for EventLog {
    type Iter<'a> = Iter<'a, Vec<Activity>>;

    fn number_of_traces(&self) -> usize {
        self.traces.len()
    }

    fn iter(&self) -> Self::Iter<'_> {
        self.traces.iter()
    }
}

impl<'a> IntoIterator for &'a EventLog {
    type Item = &'a Vec<Activity>;

    type IntoIter = Iter<'a, Vec<Activity>>;

    fn into_iter(self) -> Self::IntoIter {
        self.traces.iter()
    }
}

#[cfg(test)]
mod tests {
    use std::fs;

    use crate::{
        ActivityKey, TranslateActivityKey,
        ebi_objects::finite_stochastic_language::FiniteStochasticLanguage,
    };

    use super::EventLog;

    #[test]
    fn log_to_slang() {
        let fin = fs::read_to_string("testfiles/a-b.xes").unwrap();
        let log = fin.parse::<EventLog>().unwrap();

        let fin1 = fs::read_to_string("testfiles/a-b.slang").unwrap();
        let slang = fin1.parse::<FiniteStochasticLanguage>().unwrap();

        assert_eq!(Into::<FiniteStochasticLanguage>::into(log), slang);
    }

    #[test]
    fn log_activity_key() {
        let fin = fs::read_to_string("testfiles/a-b.xes").unwrap();
        let mut log = fin.parse::<EventLog>().unwrap();

        let mut activity_key = ActivityKey::new();
        log.translate_using_activity_key(&mut activity_key);
    }

    #[test]
    fn log_display() {
        let fin = fs::read_to_string("testfiles/a-b.xes").unwrap();
        let log = fin.parse::<EventLog>().unwrap();

        assert_eq!(format!("{}", log), "event log with 2 traces");
    }
}
