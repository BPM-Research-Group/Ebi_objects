use crate::{
    ActivityKey, EbiObject, Exportable, HasActivityKey, Importable, Infoable, IntoTraceIterator,
    NumberOfTraces, TranslateActivityKey,
    iterators::{parallel_trace_iterator::ParallelTraceIterator, trace_iterator::TraceIterator},
};
use anyhow::{Error, Result, anyhow};
use ebi_arithmetic::Fraction;
use ebi_derive::ActivityKey;
use process_mining::{
    XESImportOptions,
    event_log::{Trace, event_log_struct::EventLogClassifier},
};
use std::{
    fmt,
    io::{self, BufRead, Write},
    str::FromStr,
};

#[derive(ActivityKey, Clone)]
pub struct EventLogXes {
    pub(crate) classifier: EventLogClassifier,
    pub(crate) activity_key: ActivityKey,
    pub rust4pm_log: process_mining::EventLog,
}

impl EventLogXes {
    pub fn event_classifier(&self) -> &EventLogClassifier {
        &self.classifier
    }

    pub fn create_activity_key(&mut self) {
        self.rust4pm_log.traces.iter().for_each(|trace| {
            trace.events.iter().for_each(|event| {
                self.activity_key
                    .process_activity(&self.classifier.get_class_identity(event));
            })
        });
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
}

impl TranslateActivityKey for EventLogXes {
    fn translate_using_activity_key(&mut self, to_activity_key: &mut ActivityKey) {
        self.activity_key = to_activity_key.clone();
    }
}

impl Importable for EventLogXes {
    fn import_as_object(reader: &mut dyn BufRead) -> Result<EbiObject> {
        Ok(EbiObject::EventLogXes(Self::import(reader)?))
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
        Ok(EventLogXes::from((log, classifier)))
    }
}

impl FromStr for EventLogXes {
    type Err = Error;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        let mut reader = io::Cursor::new(s);
        Self::import(&mut reader)
    }
}

impl Exportable for EventLogXes {
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

impl fmt::Display for EventLogXes {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "event log with {} traces", self.number_of_traces())
    }
}

impl Infoable for EventLogXes {
    fn info(&self, f: &mut impl std::io::Write) -> Result<()> {
        writeln!(
            f,
            "Number of activities\t{}",
            self.activity_key().get_number_of_activities()
        )?;
        writeln!(f, "Number of traces\t{}", self.number_of_traces())?;

        let lengths = self.rust4pm_log.traces.iter().map(|t| t.events.len());
        writeln!(f, "Number of events\t{}", lengths.clone().sum::<usize>())?;
        writeln!(
            f,
            "Minimum number of events per trace\t{}",
            lengths
                .clone()
                .min()
                .map_or("n/a".to_string(), |l| l.to_string())
        )?;
        if self.number_of_traces() > 0 {
            writeln!(
                f,
                "Average number of events per trace\t{}",
                Fraction::from(lengths.clone().sum::<usize>()) / self.number_of_traces().into()
            )?;
        } else {
            writeln!(f, "Average number of events per trace\tn/a")?;
        }
        writeln!(
            f,
            "Maximum number of events per trace\t{}",
            lengths.max().map_or("n/a".to_string(), |l| l.to_string())
        )?;

        writeln!(f, "")?;
        self.activity_key().info(f)?;

        Ok(write!(f, "")?)
    }
}

impl NumberOfTraces for EventLogXes {
    fn number_of_traces(&self) -> usize {
        self.rust4pm_log.traces.len()
    }
}

impl IntoTraceIterator for EventLogXes {
    fn iter_traces(&'_ self) -> TraceIterator<'_> {
        self.into()
    }

    fn par_iter_traces(&self) -> ParallelTraceIterator<'_> {
        self.into()
    }
}
