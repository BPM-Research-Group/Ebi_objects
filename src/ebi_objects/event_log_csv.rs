use crate::{
    ActivityKey, Attribute, AttributeKey, EbiObject, HasActivityKey, Importable, Infoable,
    NumberOfTraces, TranslateActivityKey,
    traits::importable::{ImporterParameter, ImporterParameterValues},
};
use anyhow::{Result, anyhow};
use ebi_arithmetic::Fraction;
use ebi_derive::{ActivityKey, AttributeKey};
use intmap::IntMap;
use std::{
    collections::{HashMap, hash_map::Entry},
    fmt,
    io::BufRead,
};

pub const CSV_IMPORTER_PARAMETER_TRACE_ID: ImporterParameter = ImporterParameter::String {
    name: "trace_id",
    short_name: "ti",
    explanation: "The name or number of the column that contains the trace id.",
    allowed_values: None,
    default_value: "0",
};
pub const CSV_IMPORTER_PARAMETER_ACTIVITY: ImporterParameter = ImporterParameter::String {
    name: "actvitiy",
    short_name: "ec",
    explanation: "The name or number of the column that contains the activity",
    allowed_values: None,
    default_value: "1",
};
pub const CSV_IMPORTER_PARAMETER_HAS_NO_HEADER: ImporterParameter = ImporterParameter::Flag {
    name: "no_header",
    short_name: "nh",
    explanation: "Without this flag set, the first line is read as the header. 
    With this flag set, the first line is read as an event. In the latter case, the columns are given numbers, starting from 0.",
};
pub const CSV_IMPORTER_PARAMETER_SEPARATOR: ImporterParameter = ImporterParameter::String {
    name: "separator",
    short_name: "sep",
    explanation: "The character(s) that separate the columns",
    allowed_values: None,
    default_value: ",",
};
pub const CSV_IMPORTER_PARAMETER_QUOTE_CHARACTER: ImporterParameter = ImporterParameter::String {
    name: "quote_character",
    short_name: "qc",
    explanation: "The character(s) that begin and end a quote",
    allowed_values: None,
    default_value: "\"",
};

#[derive(Clone, AttributeKey, ActivityKey)]
pub struct EventLogCsv {
    pub(crate) trace_id_attribute: Attribute,
    pub(crate) activity_attribute: Attribute,
    pub(crate) activity_key: ActivityKey,
    pub(crate) attribute_key: AttributeKey,
    pub(crate) traces: HashMap<String, Vec<IntMap<Attribute, String>>>,
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
    The first row contains the names of the attributes.
    You'll likely need to set some import parameters to make importing succeed.";

    const IMPORTER_PARAMETERS: &[ImporterParameter] = &[
        CSV_IMPORTER_PARAMETER_TRACE_ID,
        CSV_IMPORTER_PARAMETER_ACTIVITY,
        CSV_IMPORTER_PARAMETER_HAS_NO_HEADER,
        CSV_IMPORTER_PARAMETER_SEPARATOR,
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
        let has_header = !parameter_values
            .get(&CSV_IMPORTER_PARAMETER_HAS_NO_HEADER)
            .ok_or_else(|| anyhow!("parameter not found"))?
            .as_bool()?;

        let separator = parameter_values
            .get(&CSV_IMPORTER_PARAMETER_SEPARATOR)
            .ok_or_else(|| anyhow!("parameter not found"))?
            .as_string()?;

        let mut csv = csv::ReaderBuilder::new()
            .has_headers(has_header)
            .from_reader(reader);

        //create the intermediate event structure
        let mut attribute_key = AttributeKey::new();
        let mut events = vec![];
        for record in csv.records() {
            let record = record?;
            let mut data = IntMap::new();
            for (column_index, cell) in record.into_iter().enumerate() {
                let attribute = attribute_key.process_attribute_column(column_index, cell);
                data.insert(attribute, cell.to_string());
            }
            events.push(data);
        }

        //update the headers if headers were requested
        if has_header {
            let record = csv.headers()?;
            for (attribute_id, head) in record.iter().enumerate() {
                attribute_key.attribute2name[attribute_id] = head.to_string();
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

        //distribute events over traces
        let mut traces: HashMap<String, Vec<IntMap<Attribute, String>>> = HashMap::new();
        for event in events {
            if let Some(trace_id) = event.get(activity_attribute) {
                match traces.entry(trace_id.to_string()) {
                    Entry::Occupied(mut occupied_entry) => occupied_entry.get_mut().push(event),
                    Entry::Vacant(vacant_entry) => {
                        vacant_entry.insert(vec![event]);
                    }
                }
            }
        }

        let mut result = Self {
            trace_id_attribute,
            activity_attribute,
            activity_key: ActivityKey::new(),
            attribute_key,
            traces,
        };

        result.create_activity_key();
        Ok(result)
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
        writeln!(
            f,
            "Number of activities\t{}",
            self.activity_key().get_number_of_activities()
        )?;
        writeln!(f, "Number of traces\t{}", self.number_of_traces())?;

        let lengths = self.traces.iter().map(|t| t.1.len());
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

impl fmt::Display for EventLogCsv {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "event log with {} traces", self.number_of_traces())
    }
}

impl NumberOfTraces for EventLogCsv {
    fn number_of_traces(&self) -> usize {
        self.traces.len()
    }
}
