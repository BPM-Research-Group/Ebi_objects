use anyhow::Result;
use chrono::{DateTime, FixedOffset};
use serde::{Deserialize, Serialize};
use std::{
    fmt::{self, Display},
    io::BufRead,
};

#[cfg(any(test, feature = "testactivities"))]
use crate::activity_key::has_activity_key::TestActivityKey;
use crate::{
    Exportable, Importable, Infoable,
    constants::ebi_object::EbiObject,
    traits::importable::{ImporterParameter, ImporterParameterValues, from_string},
};

pub const HEADER: &str = "executions";

pub const FORMAT_SPECIFICATION: &str = "not yet finalised.";

#[derive(Serialize, Deserialize, Clone)]
pub struct Executions {
    executions: Vec<Execution>,
}

impl Importable for Executions {
    const FILE_FORMAT_SPECIFICATION_LATEX: &str = "A JSON-formatted list of executions.
    
    For instance:
    \\lstinputlisting[language=ebilines, style=boxed]{../testfiles/a-b.exs}";

    const IMPORTER_PARAMETERS: &[ImporterParameter] = &[];

    fn import_as_object(
        reader: &mut dyn BufRead,
        parameter_values: &ImporterParameterValues,
    ) -> Result<EbiObject> {
        Ok(EbiObject::Executions(Self::import(
            reader,
            parameter_values,
        )?))
    }

    fn import(reader: &mut dyn BufRead, _: &ImporterParameterValues) -> Result<Self>
    where
        Self: Sized,
    {
        Ok(serde_json::from_reader(reader)?)
    }
}
from_string!(Executions);

impl Infoable for Executions {
    fn info(&self, f: &mut impl std::io::Write) -> Result<()> {
        writeln!(f, "Number of executions\t\t{}", self.executions.len())?;

        Ok(writeln!(f, "")?)
    }
}

impl Display for Executions {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let x = serde_json::to_string(self).unwrap();
        write!(f, "{}", x)
    }
}

impl Exportable for Executions {
    fn export_from_object(object: EbiObject, f: &mut dyn std::io::Write) -> Result<()> {
        match object {
            EbiObject::Executions(exe) => exe.export(f),
            _ => unreachable!(),
        }
    }

    fn export(&self, f: &mut dyn std::io::Write) -> Result<()> {
        Ok(write!(f, "{}", self)?)
    }
}

impl From<Vec<Execution>> for Executions {
    fn from(value: Vec<Execution>) -> Self {
        Self { executions: value }
    }
}

#[derive(Serialize, Deserialize, Clone)]
pub struct Execution {
    pub transition: usize,
    pub enabled_transitions_at_enablement: Option<Vec<usize>>,
    pub time_of_enablement: Option<DateTime<FixedOffset>>,
    pub time_of_execution: Option<DateTime<FixedOffset>>,
    pub features_at_enablement: Option<Vec<usize>>,
}

impl Display for Execution {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let x = serde_json::to_string(self).unwrap();
        write!(f, "{}", x)
    }
}

#[cfg(any(test, feature = "testactivities"))]
impl TestActivityKey for Executions {
    fn test_activity_key(&self) {
        //no activities are stored
    }
}