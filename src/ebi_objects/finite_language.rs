use anyhow::{Context, Error, Result, anyhow};
use ebi_derive::ActivityKey;
use fnv::FnvBuildHasher;
use rayon::iter::{IntoParallelIterator, IntoParallelRefIterator};
use std::{
    collections::{
        HashSet,
        hash_set::{IntoIter, Iter},
    },
    fmt::Display,
    io::{self, BufRead},
    str::FromStr,
};

use crate::{
    Activity, ActivityKey, ActivityKeyTranslator, Exportable, HasActivityKey, Importable,
    IndexTrace, Infoable, TranslateActivityKey,
    constants::ebi_object::EbiObject,
    line_reader::LineReader,
    traits::index_trace::{ParallelTraceIterator, TraceIterator},
};

use super::{event_log::EventLog, finite_stochastic_language::FiniteStochasticLanguage};

pub const HEADER: &str = "finite language";

pub const FORMAT_SPECIFICATION: &str =
    "A finite language is a line-based structure. Lines starting with a \\# are ignored.
    This first line is exactly `finite language'.
    The second line is the number of traces in the language.
    For each trace, the first line contains the number of events in the trace.
    Then, each subsequent line contains the activity name of one event.
    
    For instance:
    \\lstinputlisting[language=ebilines, style=boxed]{../testfiles/aa-ab-ba.lang}";

#[derive(ActivityKey, Clone)]
pub struct FiniteLanguage {
    pub activity_key: ActivityKey,
    pub traces: HashSet<Vec<Activity>, FnvBuildHasher>,
}

impl FiniteLanguage {
    pub fn new_hashmap() -> HashSet<Vec<Activity>, FnvBuildHasher> {
        HashSet::<_, FnvBuildHasher>::default()
    }

    pub fn push_string(&mut self, trace: Vec<String>) {
        self.traces.insert(self.activity_key.process_trace(&trace));
    }

    pub fn push(&mut self, trace: Vec<Activity>) {
        self.traces.insert(trace);
    }
}

impl TranslateActivityKey for FiniteLanguage {
    fn translate_using_activity_key(&mut self, to_activity_key: &mut ActivityKey) {
        let translator = ActivityKeyTranslator::new(&self.activity_key, to_activity_key);

        //a hashmap needs to be rebuilt, unfortunately
        let translated_traces: HashSet<Vec<Activity>, FnvBuildHasher> = self
            .traces
            .drain()
            .map(|trace| translator.translate_trace(&trace))
            .collect();

        // Update the traces in the language with the translated ones
        self.traces = translated_traces;

        self.activity_key = to_activity_key.clone();
    }
}

impl Exportable for FiniteLanguage {
    fn export_from_object(object: EbiObject, f: &mut dyn std::io::Write) -> Result<()> {
        match object {
            EbiObject::EventLog(log) => <EventLog as Into<FiniteLanguage>>::into(log).export(f),
            EbiObject::FiniteLanguage(slang) => slang.export(f),
            EbiObject::FiniteStochasticLanguage(slang) => {
                <FiniteStochasticLanguage as Into<FiniteLanguage>>::into(slang).export(f)
            }
            _ => Err(anyhow!(
                "Cannote export {} {} as a finite language.",
                object.get_type().get_article(),
                object.get_type()
            )),
        }
    }

    fn export(&self, f: &mut dyn std::io::Write) -> Result<()> {
        Ok(write!(f, "{}", self)?)
    }
}

impl Infoable for FiniteLanguage {
    fn info(&self, f: &mut impl std::io::Write) -> Result<()> {
        writeln!(f, "Number of traces\t{}", self.traces.len())?;
        writeln!(
            f,
            "Number of events\t{}",
            self.traces.iter().map(|t| t.len()).sum::<usize>()
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

impl Display for FiniteLanguage {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}", HEADER)?;
        writeln!(f, "# number of traces\n{}", self.number_of_traces())?;

        for (pos, trace) in self.traces.iter().enumerate() {
            writeln!(f, "# trace {}", pos)?;

            writeln!(f, "# number of events\n{}", trace.len())?;
            for activity in trace {
                writeln!(f, "{}", self.activity_key.get_activity_label(activity))?;
            }
        }

        write!(f, "")
    }
}

impl FromStr for FiniteLanguage {
    type Err = Error;

    fn from_str(s: &str) -> std::prelude::v1::Result<Self, Self::Err> {
        let mut reader = io::Cursor::new(s);
        Self::import(&mut reader)
    }
}

impl Importable for FiniteLanguage {
    fn import_as_object(reader: &mut dyn BufRead) -> Result<EbiObject> {
        Ok(EbiObject::FiniteLanguage(Self::import(reader)?))
    }

    fn import(reader: &mut dyn BufRead) -> Result<Self> {
        let mut lreader = LineReader::new(reader);

        let head = lreader
            .next_line_string()
            .with_context(|| format!("failed to read header, which should be `{}`", HEADER))?;
        if head != HEADER {
            return Err(anyhow!(
                "first line should be exactly `{}`, but found `{}`",
                HEADER,
                head
            ));
        }

        let number_of_traces = lreader
            .next_line_index()
            .context("failed to read number of places")?;

        let mut traces = HashSet::<Vec<Activity>, FnvBuildHasher>::default();
        let mut activity_key = ActivityKey::new();
        for trace_i in 0..number_of_traces {
            let number_of_events = lreader.next_line_index().with_context(|| {
                format!(
                    "failed to read number of events for trace {} at line {}",
                    trace_i,
                    lreader.get_last_line_number()
                )
            })?;

            let mut trace = vec![];
            trace.reserve_exact(number_of_events);

            for event_i in 0..number_of_events {
                let event = lreader.next_line_string().with_context(|| {
                    format!(
                        "failed to read event {} of trace {} at line {}",
                        event_i,
                        trace_i,
                        lreader.get_last_line_number()
                    )
                })?;
                trace.push(event);
            }

            let trace = activity_key.process_trace(&trace);

            if !traces.insert(trace) {
                return Err(anyhow!(
                    "trace {} ending at line {} appears twice in language",
                    trace_i,
                    lreader.get_last_line_number()
                ));
            }
        }

        Ok(Self {
            activity_key: activity_key,
            traces: traces,
        })
    }
}

impl IndexTrace for FiniteLanguage {
    fn number_of_traces(&self) -> usize {
        self.traces.len()
    }

    fn iter_traces(&self) -> TraceIterator<'_> {
        TraceIterator::HashSet(self.traces.iter())
    }

    fn par_iter_traces(&self) -> crate::traits::index_trace::ParallelTraceIterator<'_> {
        ParallelTraceIterator::HashSet(self.traces.par_iter())
    }
}

impl IntoIterator for FiniteLanguage {
    type Item = Vec<Activity>;
    type IntoIter = IntoIter<Vec<Activity>>;

    fn into_iter(self) -> Self::IntoIter {
        self.traces.into_iter()
    }
}

impl<'a> IntoIterator for &'a FiniteLanguage {
    type Item = &'a Vec<Activity>;
    type IntoIter = Iter<'a, Vec<Activity>>;

    fn into_iter(self) -> Self::IntoIter {
        self.traces.iter()
    }
}

impl IntoParallelIterator for FiniteLanguage {
    type Iter = rayon::collections::hash_set::IntoIter<Vec<Activity>>;
    type Item = Vec<Activity>;

    fn into_par_iter(self) -> Self::Iter {
        self.traces.into_par_iter()
    }
}

impl<'a> IntoParallelIterator for &'a FiniteLanguage {
    type Iter = rayon::collections::hash_set::Iter<'a, Vec<Activity>>;
    type Item = &'a Vec<Activity>;

    fn into_par_iter(self) -> Self::Iter {
        self.traces.par_iter()
    }
}
