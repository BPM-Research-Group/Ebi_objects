#[cfg(any(test, feature = "testactivities"))]
use crate::activity_key::has_activity_key::TestActivityKey;
use crate::{
    Activity, ActivityKey, EbiObject, Exportable, HasActivityKey, Importable, Infoable,
    IntoTraceIterator, NumberOfTraces, TranslateActivityKey,
    iterators::{parallel_trace_iterator::ParallelTraceIterator, trace_iterator::TraceIterator},
    log_infoable_startend, log_infoable_stats,
    traits::{
        importable::{ImporterParameter, ImporterParameterValues, from_string},
        start_end_activities::StartEndActivities,
    },
};
use anyhow::{Context, Result, anyhow};
use ebi_arithmetic::{Fraction, One};
use ebi_derive::ActivityKey;
use intmap::IntMap;
use process_mining::{
    Importable as I, OCEL,
    core::{
        event_data::object_centric::{OCELObject, ocel_xml::export_ocel_xml},
        process_models::ocpt::ObjectType,
    },
};
use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
    io::BufRead,
};

pub const OCEL_DEFAULT_OBJECT_TYPE: &str = "0";

pub const OCEL_IMPORTER_PARAMETER_OBJECT_TYPE: ImporterParameter = ImporterParameter::String {
    name: "ocel_object_type",
    short_name: "ot",
    explanation: "The object type that defines what a trace is. May be given as the name of the object-type (which takes preference) or as the index of the object-type in the order of declaration.",
    allowed_values: None,
    default_value: OCEL_DEFAULT_OBJECT_TYPE,
};

#[derive(Clone, ActivityKey)]
pub struct EventLogOcel {
    pub(crate) activity_key: ActivityKey,
    pub rust4pm_log: process_mining::OCEL,
    /// The object type that determines the traces, in case an event log is extracted.
    pub object_type: ObjectType,
}

impl EventLogOcel {
    pub fn create_activity_key(&mut self) {
        for event_type in &self.rust4pm_log.event_types {
            self.activity_key.process_activity(&event_type.name);
        }
    }

    pub fn get_relevant_objects(objects: &Vec<OCELObject>, object_type: &str) -> HashSet<String> {
        objects
            .into_iter()
            .filter_map(|ob| {
                if ob.object_type == object_type {
                    Some(ob.id.clone())
                } else {
                    None
                }
            })
            .collect::<HashSet<_>>()
    }

    pub fn get_trace_lengths(&self) -> Vec<usize> {
        let mut object_id2length = HashMap::new();
        let objects = Self::get_relevant_objects(&self.rust4pm_log.objects, &self.object_type);
        for event in &self.rust4pm_log.events {
            for relation in &event.relationships {
                if objects.contains(&relation.object_id) {
                    *object_id2length
                        .entry(relation.object_id.clone())
                        .or_insert_with(|| 0) += 1;
                }
            }
        }

        object_id2length.into_values().collect()
    }
}

impl Importable for EventLogOcel {
    const IMPORTER_PARAMETERS: &[ImporterParameter] = &[OCEL_IMPORTER_PARAMETER_OBJECT_TYPE];

    const FILE_FORMAT_SPECIFICATION_LATEX: &str = "An object-centric event log file follows the Ocel 2.0 format~\\cite{DBLP:journals/corr/abs-2403-01975}. 
Parsing is performed by the Rust4PM crate~\\cite{DBLP:conf/bpm/KustersA24}.
For instance:
    \\lstinputlisting[language=xml, style=boxed]{../testfiles/a-b.xes}";

    fn import_as_object(
        reader: &mut dyn BufRead,
        parameter_values: &ImporterParameterValues,
    ) -> Result<EbiObject> {
        Ok(EbiObject::EventLogOcel(Self::import(
            reader,
            parameter_values,
        )?))
    }

    fn import(reader: &mut dyn BufRead, parameter_values: &ImporterParameterValues) -> Result<Self>
    where
        Self: Sized,
    {
        let log = OCEL::import_from_reader_with_options(reader, "xml", ())
            .with_context(|| "cannot read Ocel XML log")?;

        //create the object-type
        let read_object_type = parameter_values
            .get(&OCEL_IMPORTER_PARAMETER_OBJECT_TYPE)
            .ok_or_else(|| anyhow!("expected parameter not found"))?
            .as_string()?;

        let object_type = if log.object_types.iter().any(|t| t.name == read_object_type) {
            read_object_type
        } else if let Ok(rank) = read_object_type.parse::<usize>()
            && rank < log.object_types.len()
        {
            log.object_types[rank].name.clone()
        } else {
            return Err(anyhow!("object-type {} not found", read_object_type));
        };

        Ok((log, object_type).into())
    }
}
from_string!(EventLogOcel);

impl Display for EventLogOcel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "ocel event log with {} events", self.number_of_events())
    }
}

impl Infoable for EventLogOcel {
    fn info(&self, f: &mut impl std::io::Write) -> Result<()> {
        println!(
            "Number of unique events\t\t{}",
            self.rust4pm_log.events.len()
        );
        println!("Number of objects\t\t\t{}", self.rust4pm_log.objects.len());

        let lengths = self.get_trace_lengths();
        log_infoable_stats!(f, self, lengths.iter());

        writeln!(f, "")?;
        self.activity_key().info(f)?;

        log_infoable_startend!(f, self);

        writeln!(
            f,
            "Event types\n\t{}",
            self.rust4pm_log
                .event_types
                .iter()
                .map(|et| et.name.as_str())
                .collect::<Vec<_>>()
                .join("\n\t")
        )?;

        writeln!(f, "")?;
        writeln!(
            f,
            "Object types\n\t{}",
            self.rust4pm_log
                .object_types
                .iter()
                .map(|ot| ot.name.as_str())
                .collect::<Vec<_>>()
                .join("\n\t")
        )?;

        Ok(writeln!(f, "")?)
    }
}

impl Exportable for EventLogOcel {
    fn export_from_object(object: EbiObject, f: &mut dyn std::io::Write) -> Result<()> {
        match object {
            EbiObject::EventLogOcel(log) => log.export(f),
            _ => Err(anyhow!("Cannot export as ocel event log.")),
        }
    }

    fn export(&self, f: &mut dyn std::io::Write) -> Result<()> {
        match export_ocel_xml(f, &self.rust4pm_log) {
            Ok(_) => Ok(()),
            Err(e) => Err(e.into()),
        }
    }
}

impl TranslateActivityKey for EventLogOcel {
    fn translate_using_activity_key(&mut self, to_activity_key: &mut ActivityKey) {
        std::mem::swap(&mut self.activity_key, to_activity_key);
        self.create_activity_key();
        let mut cloned = self.activity_key.clone();
        std::mem::swap(to_activity_key, &mut cloned);
    }
}

impl StartEndActivities for EventLogOcel {
    fn start_activities(&self) -> IntMap<Activity, Fraction> {
        let relevant_objects =
            Self::get_relevant_objects(&self.rust4pm_log.objects, &self.object_type);
        let mut started_traces = HashSet::new();
        let mut result = IntMap::new();
        for event in &self.rust4pm_log.events {
            for relation in &event.relationships {
                if relevant_objects.contains(&relation.object_id) {
                    let activity = self
                        .activity_key()
                        .process_activity_attempt(&event.event_type);
                    if let Some(activity) = activity {
                        if started_traces.insert(relation.object_id.clone()) {
                            match result.entry(activity) {
                                intmap::Entry::Occupied(mut occupied_entry) => {
                                    *occupied_entry.get_mut() += 1
                                }
                                intmap::Entry::Vacant(vacant_entry) => {
                                    vacant_entry.insert(Fraction::one());
                                }
                            }
                        }
                    }
                }
            }
        }
        result
    }

    fn end_activities(&self) -> IntMap<Activity, Fraction> {
        let relevant_objects =
            Self::get_relevant_objects(&self.rust4pm_log.objects, &self.object_type);
        let mut started_traces = HashSet::new();
        let mut result = IntMap::new();
        for event in self.rust4pm_log.events.iter().rev() {
            for relation in &event.relationships {
                if relevant_objects.contains(&relation.object_id) {
                    if started_traces.insert(relation.object_id.clone()) {
                        let activity = self
                            .activity_key()
                            .process_activity_attempt(&event.event_type);
                        if let Some(activity) = activity {
                            match result.entry(activity) {
                                intmap::Entry::Occupied(mut occupied_entry) => {
                                    *occupied_entry.get_mut() += 1
                                }
                                intmap::Entry::Vacant(vacant_entry) => {
                                    vacant_entry.insert(Fraction::one());
                                }
                            }
                        }
                    }
                }
            }
        }
        result
    }
}

impl NumberOfTraces for EventLogOcel {
    /// returns the number of traces in the log view
    fn number_of_traces(&self) -> usize {
        self.rust4pm_log.objects.iter().fold(0, |count, obj| {
            if obj.object_type == self.object_type {
                count + 1
            } else {
                count
            }
        })
    }

    /// returns the number of events in the log view
    fn number_of_events(&self) -> usize {
        self.get_trace_lengths().iter().sum()
    }
}

impl IntoTraceIterator for EventLogOcel {
    /// This trace iterator is hugely inefficient. If you can spare the memory, it is suggested to convert the Ocel into an EventLog first.
    fn iter_traces(&'_ self) -> TraceIterator<'_> {
        TraceIterator::Ocel(self.into())
    }

    /// This trace iterator is hugely inefficient. If you can spare the memory, it is suggested to convert the Ocel into an EventLog first.
    fn par_iter_traces(&self) -> ParallelTraceIterator<'_> {
        self.into()
    }
}

#[cfg(any(test, feature = "testactivities"))]
impl TestActivityKey for EventLogOcel {
    fn test_activity_key(&self) {
        //no activities are stored
    }
}
