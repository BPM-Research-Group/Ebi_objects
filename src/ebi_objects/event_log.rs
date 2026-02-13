#[cfg(any(test, feature = "testactivities"))]
use crate::activity_key::has_activity_key::TestActivityKey;
use crate::{
    Activity, ActivityKey, ActivityKeyTranslator, EventLogXes, Exportable, HasActivityKey,
    Importable, Infoable, TranslateActivityKey,
    constants::ebi_object::EbiObject,
    iterators::{
        parallel_ref_trace_iterator::ParallelRefTraceIterator, ref_trace_iterator::RefTraceIterator,
    },
    traits::{
        importable::{ImporterParameter, ImporterParameterValues, from_string},
        number_of_traces::NumberOfTraces,
        start_end_activities::StartEndActivities,
        trace_iterators::IntoRefTraceIterator,
    },
};
use anyhow::{Result, anyhow};
use core::fmt;
use ebi_arithmetic::{Fraction, One};
use ebi_derive::ActivityKey;
use intmap::IntMap;
use rayon::iter::{IntoParallelIterator, IntoParallelRefIterator, IntoParallelRefMutIterator};
use std::{
    io::{BufRead, Write},
    slice::{Iter, IterMut},
    vec::IntoIter,
};

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
    const FILE_FORMAT_SPECIFICATION_LATEX: &str = EventLogXes::FILE_FORMAT_SPECIFICATION_LATEX;

    const IMPORTER_PARAMETERS: &[ImporterParameter] = EventLogXes::IMPORTER_PARAMETERS;

    fn import_as_object(
        reader: &mut dyn BufRead,
        parameter_values: &ImporterParameterValues,
    ) -> Result<EbiObject> {
        Ok(EbiObject::EventLog(Self::import(reader, parameter_values)?))
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
from_string!(EventLog);

impl Exportable for EventLog {
    fn export_from_object(object: EbiObject, f: &mut dyn Write) -> Result<()> {
        match object {
            EbiObject::EventLog(log) => log.export(f),
            EbiObject::EventLogCsv(log) => Into::<Self>::into(log).export(f),
            EbiObject::EventLogXes(log) => Into::<Self>::into(log).export(f),
            EbiObject::EventLogTraceAttributes(log) => Into::<Self>::into(log).export(f),
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

#[macro_export]
macro_rules! log_infoable_stats {
    ($f:ident, $self:ident, $lengths:expr) => {
        writeln!(
            $f,
            "Number of activities\t\t\t{}",
            $self.activity_key().get_number_of_activities()
        )?;
        writeln!($f, "Number of traces\t\t\t{}", $self.number_of_traces())?;
        writeln!($f, "Number of events\t\t\t{}", $self.number_of_events())?;
        writeln!(
            $f,
            "Number of activities\t\t\t{}",
            $self.activity_key().get_number_of_activities()
        )?;
        writeln!(
            $f,
            "Minimum number of events per trace\t{}",
            $lengths
                .clone()
                .min()
                .map_or("n/a".to_string(), |l| l.to_string())
        )?;
        if $self.number_of_traces() > 0 {
            writeln!(
                $f,
                "Average number of events per trace\t{}",
                Fraction::from($self.number_of_events()) / Fraction::from($self.number_of_traces())
            )?;
        } else {
            writeln!($f, "Average number of events per trace\tn/a")?;
        }
        writeln!(
            $f,
            "Maximum number of events per trace\t{}",
            $lengths.max().map_or("n/a".to_string(), |l| l.to_string())
        )?;
    };
}

#[macro_export]
macro_rules! log_infoable_startend {
    ($f:ident, $self:ident) => {
        writeln!($f, "")?;
        writeln!($f, "Start activities")?;
        for (activity, cardinality) in $self.start_activities() {
            writeln!(
                $f,
                "\t{}: {}",
                $self.activity_key.get_activity_label(&activity),
                cardinality
            )?;
        }

        writeln!($f, "")?;
        writeln!($f, "End activities")?;
        for (activity, cardinality) in $self.end_activities() {
            writeln!(
                $f,
                "\t{}: {}",
                $self.activity_key.get_activity_label(&activity),
                cardinality
            )?;
        }
    };
}

impl Infoable for EventLog {
    fn info(&self, f: &mut impl std::io::Write) -> Result<()> {
        let lengths = self.traces.iter().map(|t| t.len());

        log_infoable_stats!(f, self, lengths);

        writeln!(f, "")?;
        self.activity_key().info(f)?;

        log_infoable_startend!(f, self);

        Ok(writeln!(f, "")?)
    }
}

impl NumberOfTraces for EventLog {
    fn number_of_traces(&self) -> usize {
        self.traces.len()
    }

    fn number_of_events(&self) -> usize {
        self.traces.iter().map(|t| t.len()).sum()
    }
}

impl StartEndActivities for EventLog {
    fn start_activities(&self) -> IntMap<Activity, Fraction> {
        let mut result = IntMap::new();
        for trace in self.traces.iter() {
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
        for trace in self.traces.iter() {
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

impl IntoRefTraceIterator for EventLog {
    fn iter_traces(&self) -> RefTraceIterator<'_> {
        RefTraceIterator::Vec(self.traces.iter())
    }

    fn par_iter_traces(&self) -> ParallelRefTraceIterator<'_> {
        ParallelRefTraceIterator::Vec(self.traces.par_iter())
    }
}

impl IntoIterator for EventLog {
    type Item = Vec<Activity>;
    type IntoIter = IntoIter<Vec<Activity>>;

    fn into_iter(self) -> Self::IntoIter {
        self.traces.into_iter()
    }
}

impl<'a> IntoIterator for &'a EventLog {
    type Item = &'a Vec<Activity>;
    type IntoIter = Iter<'a, Vec<Activity>>;

    fn into_iter(self) -> Self::IntoIter {
        self.traces.iter()
    }
}

impl<'a> IntoIterator for &'a mut EventLog {
    type Item = &'a mut Vec<Activity>;
    type IntoIter = IterMut<'a, Vec<Activity>>;

    fn into_iter(self) -> Self::IntoIter {
        self.traces.iter_mut()
    }
}

impl IntoParallelIterator for EventLog {
    type Iter = rayon::vec::IntoIter<Vec<Activity>>;
    type Item = Vec<Activity>;

    fn into_par_iter(self) -> Self::Iter {
        self.traces.into_par_iter()
    }
}

impl<'a> IntoParallelIterator for &'a EventLog {
    type Iter = rayon::slice::Iter<'a, Vec<Activity>>;
    type Item = &'a Vec<Activity>;

    fn into_par_iter(self) -> Self::Iter {
        self.traces.par_iter()
    }
}

impl<'a> IntoParallelIterator for &'a mut EventLog {
    type Iter = rayon::slice::IterMut<'a, Vec<Activity>>;
    type Item = &'a mut Vec<Activity>;

    fn into_par_iter(self) -> Self::Iter {
        self.traces.par_iter_mut()
    }
}

#[cfg(any(test, feature = "testactivities"))]
impl TestActivityKey for EventLog {
    fn test_activity_key(&self) {
        self.traces.iter().for_each(|trace| {
            trace
                .iter()
                .for_each(|activity| self.activity_key().assert_activity_is_of_key(activity))
        });
    }
}

#[cfg(test)]
mod tests {
    use super::EventLog;
    use crate::{
        ActivityKey, TranslateActivityKey,
        ebi_objects::finite_stochastic_language::FiniteStochasticLanguage,
    };
    use std::fs;

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
