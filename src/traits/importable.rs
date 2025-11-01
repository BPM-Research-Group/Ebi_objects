use anyhow::{Result, anyhow};
use ebi_arithmetic::{ConstFraction, Fraction};
use std::{collections::HashMap, hash::Hash, io::BufRead};
use strum_macros::Display;

use crate::constants::ebi_object::EbiObject;

pub trait Importable {
    ///Defines the parameters that the importer expects.
    const IMPORTER_PARAMETERS: &[ImporterParameter];

    ///A latex piece that describes the file format.
    const FILE_FORMAT_SPECIFICATION_LATEX: &str;

    fn import_as_object(
        reader: &mut dyn BufRead,
        parameter_values: &ImporterParameterValues,
    ) -> Result<EbiObject>;

    ///Attempts to import from the reader.
    ///The parameter_values must match the order given in `IMPORTER_PARAMETERS`.
    fn import(reader: &mut dyn BufRead, parameter_values: &ImporterParameterValues) -> Result<Self>
    where
        Self: Sized;

    fn default_importer_parameter_values() -> ImporterParameterValues {
        let mut result = HashMap::new();
        for parameter in Self::IMPORTER_PARAMETERS {
            result.insert(*parameter, parameter.default());
        }
        result
    }
}

macro_rules! from_string {
    ($t:ident) => {
        impl std::str::FromStr for $t {
            type Err = anyhow::Error;

            fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
                let mut reader = std::io::Cursor::new(s);
                let default_parameter_values = $t::default_importer_parameter_values();
                Self::import(&mut reader, &default_parameter_values)
            }
        }
    };
}
pub(crate) use from_string;

pub type ImporterParameterValues = HashMap<ImporterParameter, ImporterParameterValue>;

/// Parameters that can be given to an importer.
/// It is a design decision that every parameter must have a default,
/// to ensure every importer (also) works without user interaction.
#[derive(Copy, Clone, Debug, Display)]
pub enum ImporterParameter {
    Flag {
        name: &'static str,
        short_name: &'static str,
        explanation: &'static str,
    },
    String {
        name: &'static str,
        short_name: &'static str,
        explanation: &'static str,
        allowed_values: Option<&'static [&'static str]>,
        default_value: &'static str,
    },
    Usize {
        name: &'static str,
        short_name: &'static str,
        explanation: &'static str,
        minimum_value: Option<usize>,
        maximum_value: Option<usize>,
        default_value: usize,
    },
    Fraction {
        name: &'static str,
        short_name: &'static str,
        explanation: &'static str,
        minimum_value: Option<ConstFraction>,
        maximum_value: Option<ConstFraction>,
        default_value: ConstFraction,
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

    pub fn short_name(&self) -> &'static str {
        match self {
            ImporterParameter::Flag { short_name, .. }
            | ImporterParameter::String { short_name, .. }
            | ImporterParameter::Usize { short_name, .. }
            | ImporterParameter::Fraction { short_name, .. } => short_name,
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

    pub fn default(&self) -> ImporterParameterValue {
        match self {
            ImporterParameter::Flag { .. } => ImporterParameterValue::Boolean(false),
            ImporterParameter::String { default_value, .. } => {
                ImporterParameterValue::String(default_value.to_string())
            }
            ImporterParameter::Usize { default_value, .. } => {
                ImporterParameterValue::Usize(*default_value)
            }
            ImporterParameter::Fraction { default_value, .. } => {
                ImporterParameterValue::Fraction(default_value.to_fraction())
            }
        }
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

impl Ord for ImporterParameter {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.name().cmp(other.name())
    }
}

impl PartialOrd for ImporterParameter {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.name().partial_cmp(other.name())
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
    pub fn as_string(&self) -> Result<String> {
        match self {
            ImporterParameterValue::String(s) => Ok(s.clone()),
            _ => Err(anyhow!("cannot read importer parameter as string")),
        }
    }

    pub fn as_bool(&self) -> Result<bool> {
        match self {
            ImporterParameterValue::Boolean(s) => Ok(*s),
            _ => Err(anyhow!("cannot read importer parameter as bool")),
        }
    }
}
