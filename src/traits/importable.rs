use anyhow::{Result, anyhow};
use ebi_arithmetic::{ConstFraction, Fraction};
use std::{collections::HashMap, hash::Hash, io::BufRead};

use crate::constants::ebi_object::EbiObject;

pub trait Importable {
    ///Defines the parameters that the importer expects.
    const IMPORTER_PARAMETERS: &[ImporterParameter];

    ///A latex piece that describes the file format.
    const FILE_FORMAT_SPECIFICATION_LATEX: &str;

    fn import_as_object(
        reader: &mut dyn BufRead,
        parameter_values: ImporterParameterValues,
    ) -> Result<EbiObject>;

    ///Attempts to import from the reader.
    ///The parameter_values must match the order given in `IMPORTER_PARAMETERS`.
    fn import(reader: &mut dyn BufRead, parameter_values: ImporterParameterValues) -> Result<Self>
    where
        Self: Sized;
}

macro_rules! from_string {
    ($t:ident) => {
        impl std::str::FromStr for $t {
            type Err = anyhow::Error;

            fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
                let mut reader = std::io::Cursor::new(s);
                let default_parameter_values = crate::traits::importable::Defaulter::default(
                    Self::IMPORTER_PARAMETERS,
                )
                .ok_or_else(|| {
                    anyhow::anyhow!(
                        "cannot import from string as not all parameters have default values"
                    )
                })?;
                Self::import(&mut reader, default_parameter_values)
            }
        }
    };
}
pub(crate) use from_string;

pub type ImporterParameterValues = HashMap<ImporterParameter, ImporterParameterValue>;

#[derive(Copy, Clone)]
pub enum ImporterParameter {
    Flag {
        name: &'static str,
        explanation: &'static str,
    },
    String {
        name: &'static str,
        explanation: &'static str,
        allowed_values: Option<&'static [&'static str]>,
        default_value: Option<&'static str>,
    },
    Usize {
        name: &'static str,
        explanation: &'static str,
        minimum_value: Option<usize>,
        maximum_value: Option<usize>,
        default_value: Option<usize>,
    },
    Fraction {
        name: &'static str,
        explanation: &'static str,
        minimum_value: Option<ConstFraction>,
        maximum_value: Option<ConstFraction>,
        default_value: Option<ConstFraction>,
    },
}

impl ImporterParameter {
    pub fn name(&self) -> &'static str {
        match self {
            ImporterParameter::Flag { name, .. }
            | ImporterParameter::String { name, .. }
            | ImporterParameter::Usize { name, .. }
            | ImporterParameter::Fraction { name, .. } => name,
        }
    }

    pub fn explanation(&self) -> &'static str {
        match self {
            ImporterParameter::Flag { explanation, .. }
            | ImporterParameter::String { explanation, .. }
            | ImporterParameter::Usize { explanation, .. }
            | ImporterParameter::Fraction { explanation, .. } => explanation,
        }
    }

    pub fn default(&self) -> Option<ImporterParameterValue> {
        match self {
            ImporterParameter::Flag { .. } => Some(ImporterParameterValue::Boolean(false)),
            ImporterParameter::String { default_value, .. } => Some(
                ImporterParameterValue::String((*default_value)?.to_string()),
            ),
            ImporterParameter::Usize { default_value, .. } => Some(ImporterParameterValue::String(
                (*default_value)?.to_string(),
            )),
            ImporterParameter::Fraction { default_value, .. } => Some(
                ImporterParameterValue::String((*default_value)?.to_string()),
            ),
        }
    }
}

pub trait Defaulter {
    fn default(self) -> Option<ImporterParameterValues>;
}

impl Defaulter for &[ImporterParameter] {
    fn default(self) -> Option<ImporterParameterValues> {
        let mut result = HashMap::new();
        for parameter in self {
            result.insert(*parameter, parameter.default()?);
        }
        Some(result)
    }
}

impl Eq for ImporterParameter {}

impl PartialEq for ImporterParameter {
    fn eq(&self, other: &Self) -> bool {
        self.name() == other.name()
    }
}

impl Hash for ImporterParameter {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name().hash(state)
    }
}

#[derive(Clone)]
pub enum ImporterParameterValue {
    Boolean(bool),
    String(String),
    Usize(usize),
    Fraction(Fraction),
}

impl ImporterParameterValue {
    pub fn as_string(self) -> Result<String> {
        match self {
            ImporterParameterValue::String(s) => Ok(s),
            _ => Err(anyhow!("cannot read importer parameter as string")),
        }
    }
}
