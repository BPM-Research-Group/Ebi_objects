use crate::{
    Activity, ActivityKey, EbiObject, Exportable, HasActivityKey, Importable, Infoable,
    IntoTraceIterator, NumberOfTraces, TranslateActivityKey,
    iterators::{parallel_trace_iterator::ParallelTraceIterator, trace_iterator::TraceIterator},
    traits::importable::{ImporterParameter, ImporterParameterValues, from_string},
};
use anyhow::{Result, anyhow};
use ebi_arithmetic::Fraction;
use ebi_derive::ActivityKey;
use process_mining::{
    XESImportOptions,
    event_log::{Event, Trace, event_log_struct::EventLogClassifier},
};
use std::{
    fmt,
    io::{BufRead, Write},
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

    pub fn retain_traces<'a>(&'a mut self, f: Box<dyn Fn(&Vec<Activity>) -> bool + 'static>) {
        let mut activity_key = self.activity_key().clone();

        let event_classifier = self.event_classifier().clone();

        self.rust4pm_log.traces.retain(|trace| {
            let mut result = Vec::with_capacity(trace.events.len());

            for event in trace.events.iter() {
                let activity =
                    activity_key.process_activity(&event_classifier.get_class_identity(event));

                result.push(activity);
            }

            f(&result)
        });
    }

    pub fn get_event(&self, trace_index: usize, event_index: usize) -> Option<&Event> {
        self.rust4pm_log
            .traces
            .get(trace_index)?
            .events
            .get(event_index)
    }
}

impl TranslateActivityKey for EventLogXes {
    fn translate_using_activity_key(&mut self, to_activity_key: &mut ActivityKey) {
        self.activity_key = to_activity_key.clone();
    }
}

impl Importable for EventLogXes {
    const FILE_FORMAT_SPECIFICATION_LATEX: &str =
        "An event log file follows the IEEE XES format~\\cite{DBLP:journals/cim/AcamporaVSAGV17}. 
Parsing is performed by the Rust4PM crate~\\cite{DBLP:conf/bpm/KustersA24}.
For instance:
    \\lstinputlisting[language=xml, style=boxed]{../testfiles/a-b.xes}";

    const IMPORTER_PARAMETERS: &[ImporterParameter] = &[ImporterParameter::String {
        name: "event_classifier",
        explanation: "the attribute that defines, for each event, what its activity is",
        allowed_values: None,
        default_value: "concept:name",
    }];

    fn import_as_object(
        reader: &mut dyn BufRead,
        parameter_values: ImporterParameterValues,
    ) -> Result<EbiObject> {
        Ok(EbiObject::EventLogXes(Self::import(
            reader,
            parameter_values,
        )?))
    }

    fn import(
        reader: &mut dyn BufRead,
        parameter_values: ImporterParameterValues,
    ) -> anyhow::Result<Self>
    where
        Self: Sized,
    {
        let log =
            process_mining::event_log::import_xes::import_xes(reader, XESImportOptions::default());
        let log = match log {
            Ok(l) => l,
            Err(e) => return Err(anyhow!("{}", e)),
        };

        //create the classifier
        let key = parameter_values
            .get(&Self::IMPORTER_PARAMETERS[0])
            .ok_or_else(|| anyhow!("expected parameter not found"))?;
        let classifier = EventLogClassifier {
            name: key.clone().as_string()?,
            keys: vec![key.clone().as_string()?],
        };

        Ok((log, classifier).into())
    }
}
from_string!(EventLogXes);

impl Exportable for EventLogXes {
    fn export_from_object(object: EbiObject, f: &mut dyn Write) -> Result<()> {
        match object {
            EbiObject::EventLogXes(log) => log.export(f),
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
