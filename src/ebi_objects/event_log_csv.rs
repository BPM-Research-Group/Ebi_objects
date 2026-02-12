use crate::{
    Activity, ActivityKey, Attribute, AttributeKey, EbiObject, Exportable, HasActivityKey,
    Importable, Infoable, IntoTraceIterator, NumberOfTraces, TranslateActivityKey,
    iterators::{parallel_trace_iterator::ParallelTraceIterator, trace_iterator::TraceIterator},
    log_infoable_startend, log_infoable_stats,
    traits::{
        importable::{ImporterParameter, ImporterParameterValues, from_string},
        start_end_activities::StartEndActivities,
    },
};
use anyhow::{Context, Result, anyhow};
use ebi_arithmetic::{Fraction, One};
use ebi_derive::{ActivityKey, AttributeKey};
use intmap::IntMap;
use std::{
    collections::{HashMap, hash_map::Entry},
    fmt,
    io::{BufRead, Write},
};

pub const DEFAULT_SEPARATOR: &str = ",";
pub const DEFAULT_QUOTE_CHARACTER: &str = "\"";

pub const CSV_IMPORTER_PARAMETER_TRACE_ID: ImporterParameter = ImporterParameter::String {
    name: "csv_trace_id",
    short_name: "ti",
    explanation: "The name or number of the column that contains the trace id",
    allowed_values: None,
    default_value: "0",
};
pub const CSV_IMPORTER_PARAMETER_ACTIVITY: ImporterParameter = ImporterParameter::String {
    name: "csv_activity",
    short_name: "ec",
    explanation: "The name or number of the column that contains the activity",
    allowed_values: None,
    default_value: "1",
};
pub const CSV_IMPORTER_PARAMETER_HAS_NO_HEADER: ImporterParameter = ImporterParameter::Flag {
    name: "csv_no_header",
    short_name: "nh",
    explanation: "Without this flag set, the first line is read as the header. 
    With this flag set, the first line is read as an event. In the latter case, the columns are given numbers, starting from 0",
};
pub const CSV_IMPORTER_PARAMETER_SEPARATOR: ImporterParameter = ImporterParameter::String {
    name: "csv_separator",
    short_name: "sep",
    explanation: "The character(s) that separate the columns",
    allowed_values: None,
    default_value: DEFAULT_SEPARATOR,
};
pub const CSV_IMPORTER_PARAMETER_QUOTE_CHARACTER: ImporterParameter = ImporterParameter::String {
    name: "csv_quote_character",
    short_name: "qc",
    explanation: "The character(s) that begin and end a quote",
    allowed_values: None,
    default_value: DEFAULT_QUOTE_CHARACTER,
};

#[derive(Clone, AttributeKey, ActivityKey)]
pub struct EventLogCsv {
    pub(crate) activity_attribute: Attribute,
    pub(crate) activity_key: ActivityKey,
    pub(crate) attribute_key: AttributeKey,
    pub(crate) traces: Vec<(String, Vec<IntMap<Attribute, String>>)>,

    pub(crate) separator: u8,
    pub(crate) quote_character: u8,
}

impl EventLogCsv {
    pub fn create_activity_key(&mut self) {
        self.traces.iter().for_each(|(_, trace)| {
            trace.iter().for_each(|event| {
                if let Some(activity) = event.get(self.activity_attribute) {
                    self.activity_key.process_activity(activity);
                }
            })
        });
    }
}

impl Importable for EventLogCsv {
    const FILE_FORMAT_SPECIFICATION_LATEX: &str =
        "A text file, of which each row is a comma-separated list of attribute values. 
    The first row contains the names of the attributes (unless the no-header flag is provided).
    You'll likely need to set some import parameters to make importing succeed.
    A restriction imposed by Ebi is that there must be at least two attributes and at least two events.
    For instance:
    \\lstinputlisting[style=boxed]{../testfiles/a-b.csv}";

    const IMPORTER_PARAMETERS: &[ImporterParameter] = &[
        CSV_IMPORTER_PARAMETER_TRACE_ID,
        CSV_IMPORTER_PARAMETER_ACTIVITY,
        CSV_IMPORTER_PARAMETER_HAS_NO_HEADER,
        CSV_IMPORTER_PARAMETER_SEPARATOR,
        CSV_IMPORTER_PARAMETER_QUOTE_CHARACTER,
    ];

    fn import_as_object(
        reader: &mut dyn BufRead,
        parameter_values: &ImporterParameterValues,
    ) -> Result<EbiObject> {
        Ok(EbiObject::EventLogCsv(Self::import(
            reader,
            parameter_values,
        )?))
    }

    fn import(reader: &mut dyn BufRead, parameter_values: &ImporterParameterValues) -> Result<Self>
    where
        Self: Sized,
    {
        //create csv reader
        let has_header;
        let separator;
        let quote_character;
        let mut csv = {
            //base reader
            let mut csv = csv::ReaderBuilder::new();

            //headers
            if parameter_values
                .get(&CSV_IMPORTER_PARAMETER_HAS_NO_HEADER)
                .ok_or_else(|| anyhow!("parameter not found"))?
                .as_bool()?
            {
                csv.has_headers(false);
                has_header = false;
            } else {
                csv.has_headers(true);
                has_header = true;
            }

            //separator
            let sep = parameter_values
                .get(&CSV_IMPORTER_PARAMETER_SEPARATOR)
                .ok_or_else(|| anyhow!("parameter not found"))?
                .as_string()?;
            if sep.as_bytes().len() != 1 {
                return Err(anyhow!(
                    "separator must be a one one-byte character, found `{}`",
                    sep
                ));
            }
            separator = sep.as_bytes()[0];
            csv.delimiter(separator);

            //quote character
            let quote = parameter_values
                .get(&CSV_IMPORTER_PARAMETER_QUOTE_CHARACTER)
                .ok_or_else(|| anyhow!("parameter not found"))?
                .as_string()?;
            if quote.as_bytes().len() != 1 {
                return Err(anyhow!(
                    "quote character must be a one one-byte character, found `{}`",
                    quote
                ));
            }
            quote_character = quote.as_bytes()[0];
            csv.quote(quote_character);

            csv
        }
        .from_reader(reader);

        //create the intermediate event structure
        let mut attribute_key = AttributeKey::new();
        let mut events = vec![];
        for record in csv.records() {
            let record = record?;

            //To prevent false positives, we add a restriction to .csv files: they must contain at least 2 columns.
            if record.len() <= 1 {
                return Err(anyhow!("the csv must contain at least 2 columns"));
            }

            let mut data = IntMap::new();
            for (column_index, cell) in record.into_iter().enumerate() {
                let attribute = attribute_key.process_attribute_column(column_index, cell);
                data.insert(attribute, cell.to_string());
            }
            events.push(data);
        }

        if events.len() <= 1 {
            return Err(anyhow!("the csv must contain at least 2 events"));
        }

        //update the headers if headers were requested
        if has_header {
            let record = csv.headers()?;
            for (attribute_id, head) in record.iter().enumerate() {
                attribute_key.set_label(attribute_key.id_to_attribute(attribute_id), head);
            }
        }

        //find the trace id column
        let trace_id_attribute = {
            let parameter = parameter_values
                .get(&CSV_IMPORTER_PARAMETER_TRACE_ID)
                .ok_or_else(|| anyhow!("parameter not found"))?
                .as_string()?;
            if let Some(attribute) = attribute_key.label_to_attribute(&parameter) {
                attribute
            } else if let Ok(attribute_id) = parameter.parse::<usize>() {
                attribute_key.id_to_attribute(attribute_id)
            } else {
                return Err(anyhow!(
                    "trace id attribute `{}` not found as a column",
                    parameter
                ));
            }
        };

        //find the activity column
        let activity_attribute = {
            let parameter = parameter_values
                .get(&CSV_IMPORTER_PARAMETER_ACTIVITY)
                .ok_or_else(|| anyhow!("parameter not found"))?
                .as_string()?;
            if let Some(attribute) = attribute_key.label_to_attribute(&parameter) {
                attribute
            } else if let Ok(attribute_id) = parameter.parse::<usize>() {
                attribute_key.id_to_attribute(attribute_id)
            } else {
                return Err(anyhow!(
                    "activity attribute `{}` not found as a column",
                    parameter
                ));
            }
        };

        //combine events into traces
        let mut traces: HashMap<String, Vec<IntMap<Attribute, String>>> = HashMap::new();
        for event in events {
            if let Some(trace_id) = event.get(trace_id_attribute) {
                match traces.entry(trace_id.to_string()) {
                    Entry::Occupied(mut occupied_entry) => occupied_entry.get_mut().push(event),
                    Entry::Vacant(vacant_entry) => {
                        vacant_entry.insert(vec![event]);
                    }
                }
            }
        }
        let traces = traces.into_iter().collect();

        let mut result = Self {
            activity_attribute,
            activity_key: ActivityKey::new(),
            attribute_key,
            traces,
            separator,
            quote_character,
        };

        result.create_activity_key();
        Ok(result)
    }
}
from_string!(EventLogCsv);

impl Exportable for EventLogCsv {
    fn export_from_object(object: EbiObject, f: &mut dyn Write) -> Result<()> {
        match object {
            EbiObject::EventLog(log) => {
                let csv: Self = log
                    .try_into()
                    .with_context(|| anyhow!("Cannot transform event log to csv."))?;
                csv.export(f)
            }
            EbiObject::EventLogTraceAttributes(log) => {
                let csv: Self = log
                    .try_into()
                    .with_context(|| anyhow!("Cannot transform event log to csv."))?;
                csv.export(f)
            }
            EbiObject::EventLogXes(log) => {
                let csv: Self = log
                    .try_into()
                    .with_context(|| anyhow!("Cannot transform event log to csv."))?;
                csv.export(f)
            }
            EbiObject::EventLogCsv(log) => log.export(f),
            _ => Err(anyhow!("Cannot export as csv event log.")),
        }
    }

    fn export(&self, f: &mut dyn Write) -> Result<()> {
        let mut wtr = csv::WriterBuilder::new()
            .delimiter(self.separator)
            .quote(self.quote_character)
            .from_writer(f);

        //write header
        for attribute_label in &self.attribute_key.attribute2name {
            wtr.write_field(attribute_label)?;
        }
        wtr.write_record(None::<&[u8]>)?;

        //write events
        for (_, events) in &self.traces {
            for event in events {
                for attribute_id in 0..self.attribute_key.len() {
                    if let Some(value) = event.get(self.attribute_key.id_to_attribute(attribute_id))
                    {
                        wtr.write_field(value)?;
                    } else {
                        wtr.write_field("")?;
                    }
                }
                wtr.write_record(None::<&[u8]>)?;
            }
        }

        wtr.flush()?;
        Ok(())
    }
}

impl TranslateActivityKey for EventLogCsv {
    fn translate_using_activity_key(&mut self, to_activity_key: &mut ActivityKey) {
        std::mem::swap(&mut self.activity_key, to_activity_key);
        self.create_activity_key();
        let mut cloned = self.activity_key.clone();
        std::mem::swap(to_activity_key, &mut cloned);
    }
}

impl Infoable for EventLogCsv {
    fn info(&self, f: &mut impl std::io::Write) -> Result<()> {
        let lengths = self.traces.iter().map(|t| t.1.len());
        log_infoable_stats!(f, self, lengths);

        writeln!(f, "")?;
        self.activity_key().info(f)?;

        log_infoable_startend!(f, self);

        Ok(writeln!(f, "")?)
    }
}

impl fmt::Display for EventLogCsv {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "event log with {} traces", self.number_of_traces())
    }
}

impl NumberOfTraces for EventLogCsv {
    fn number_of_traces(&self) -> usize {
        self.traces.len()
    }

    fn number_of_events(&self) -> usize {
        self.traces.iter().map(|(_, t)| t.len()).sum()
    }
}

impl StartEndActivities for EventLogCsv {
    fn start_activities(&self) -> IntMap<Activity, Fraction> {
        let mut result = IntMap::new();
        for (_, trace) in self.traces.iter() {
            if let Some(event) = trace.iter().next() {
                let activity_label = event.get(self.activity_attribute);
                if let Some(activity_label) = activity_label {
                    let activity = self.activity_key.process_activity_attempt(&activity_label);
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
        result
    }

    fn end_activities(&self) -> IntMap<crate::Activity, Fraction> {
        let mut result = IntMap::new();
        for (_, trace) in self.traces.iter() {
            if let Some(event) = trace.iter().last() {
                let activity_label = event.get(self.activity_attribute);
                if let Some(activity_label) = activity_label {
                    let activity = self.activity_key.process_activity_attempt(&activity_label);
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
        result
    }
}

impl IntoTraceIterator for EventLogCsv {
    fn iter_traces(&'_ self) -> TraceIterator<'_> {
        TraceIterator::Csv(self.into())
    }

    fn par_iter_traces(&self) -> ParallelTraceIterator<'_> {
        self.into()
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        Importable, NumberOfTraces,
        activity_key::has_activity_key::TestActivityKey,
        ebi_objects::event_log_csv::{
            CSV_IMPORTER_PARAMETER_SEPARATOR, CSV_IMPORTER_PARAMETER_TRACE_ID, EventLogCsv,
        },
        traits::importable::ImporterParameterValue,
    };
    use std::{
        fs::{self, File},
        io::BufReader,
    };

    impl TestActivityKey for EventLogCsv {
        fn test_activity_key(&self) {
            //no activities are stored; nothing to test
        }
    }

    #[test]
    fn csv_parameters() {
        //default
        let fin = fs::read_to_string("testfiles/a-b_multiple_separators.csv").unwrap();
        let csv: EventLogCsv = fin.parse::<EventLogCsv>().unwrap();
        assert_eq!(csv.number_of_traces(), 1);

        //parse again with a different trace id column
        let mut parameter_values = EventLogCsv::default_importer_parameter_values();
        parameter_values.insert(
            CSV_IMPORTER_PARAMETER_TRACE_ID,
            ImporterParameterValue::String("alternative_case_id".to_string()),
        );
        let csv = EventLogCsv::import(
            &mut BufReader::new(File::open("testfiles/a-b_multiple_separators.csv").unwrap()),
            &parameter_values,
        )
        .unwrap();
        assert_eq!(csv.number_of_traces(), 2);

        //parse again with a different separator (invalid)
        parameter_values.insert(
            CSV_IMPORTER_PARAMETER_SEPARATOR,
            ImporterParameterValue::String("alternative_case_id".to_string()),
        );
        assert!(
            EventLogCsv::import(
                &mut BufReader::new(File::open("testfiles/a-b_multiple_separators.csv").unwrap()),
                &parameter_values,
            )
            .is_err()
        );

        //parse again with a different separator
        parameter_values.insert(
            CSV_IMPORTER_PARAMETER_SEPARATOR,
            ImporterParameterValue::String(";".to_string()),
        );
        parameter_values.insert(
            CSV_IMPORTER_PARAMETER_TRACE_ID,
            ImporterParameterValue::String("case_column".to_string()),
        );
        let csv = EventLogCsv::import(
            &mut BufReader::new(File::open("testfiles/a-b_multiple_separators.csv").unwrap()),
            &parameter_values,
        )
        .unwrap();
        assert_eq!(csv.number_of_traces(), 1);
    }

    #[test]
    fn csv_restrictive() {
        let fin = fs::read_to_string("testfiles/a-b.xes").unwrap();
        assert!(fin.parse::<EventLogCsv>().is_err());

        let fin = fs::read_to_string("testfiles/a-b.exs").unwrap();
        assert!(fin.parse::<EventLogCsv>().is_err());
    }
}
