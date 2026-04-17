use crate::{
    Exportable, Importable, Infoable,
    constants::ebi_object::EbiObject,
    json,
    traits::importable::{ImporterParameter, ImporterParameterValues, from_string},
};
use anyhow::{Context, anyhow};
use chrono::{DateTime, FixedOffset};
use ebi_activity_key::{Activity, ActivityKey, ActivityKeyTranslator, TranslateActivityKey};
#[cfg(any(test, feature = "testactivities"))]
use ebi_activity_key::{HasActivityKey, TestActivityKey};
use ebi_arithmetic::anyhow::{Error, Result};
use ebi_derive::ActivityKey;
use serde_json::{Number, Value};
use std::{
    fmt::{self, Display},
    io::BufRead,
};

pub const JSON_KEY_EXECUTIONS: &str = "executions";
pub const JSON_KEY_ACTIVITY: &str = "activity";
pub const JSON_KEY_ALSO_IN_LOG: &str = "also_in_log";
pub const JSON_KEY_ENABLED_TRANSITIONS: &str = "enabled_transitions";
pub const JSON_KEY_TIME_OF_ENABLEMENT: &str = "time_of_enablement";
pub const JSON_KEY_TIME_OF_EXECUTION: &str = "time_of_execution";
pub const JSON_KEY_RESOURCE: &str = "resource";

#[derive(Clone, ActivityKey)]
pub struct Executions {
    pub(crate) activity_key: ActivityKey,
    pub(crate) resource_key: ActivityKey,
    pub executions: Vec<Execution>,
}

impl TranslateActivityKey for Executions {
    fn translate_using_activity_key(&mut self, to_activity_key: &mut ActivityKey) {
        let translator = ActivityKeyTranslator::new(&self.activity_key, to_activity_key);

        self.executions.iter_mut().for_each(|execution| {
            if let Some(mut activity) = execution.activity {
                translator.translate_activity(&mut activity);
            }
        });

        self.activity_key = to_activity_key.clone();
    }
}

impl Importable for Executions {
    const FILE_FORMAT_SPECIFICATION_LATEX: &str = "An executions file is a JSON structure, of which the root is an object with one element called `executions`.
    This is a list of executions, where each execution contains:
    \\begin{itemize}
        \\item An activity label (may be null), 
        \\item a list of enabled transitions of which the first one actually fired, 
        \\item a resource (may be null),
        \\item a time of enablement (may be null), and 
        \\item a time of execution (may be null).
    \\end{itemize} 
    
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
        let json: Value = serde_json::from_reader(reader)?;
        let json_executions = json::read_field_list(&json, JSON_KEY_EXECUTIONS)
            .with_context(|| anyhow!("Could not read list named `{}`.", JSON_KEY_EXECUTIONS))?;

        let mut activity_key = ActivityKey::new();
        let mut resource_key = ActivityKey::new();
        let mut executions = Vec::with_capacity(json_executions.len());

        for (execution_i, json_execution) in json_executions.into_iter().enumerate() {
            //activity
            let activity = if let Some(activity) =
                json::read_field_string_or_null(&json_execution, "activity").with_context(|| {
                    anyhow!("Could not reach activity of execution {}.", execution_i)
                })? {
                Some(activity_key.process_activity(&activity))
            } else {
                None
            };

            //also in log
            let also_in_log = json::read_field_bool(json_execution, JSON_KEY_ALSO_IN_LOG)
                .with_context(|| {
                    anyhow!(
                        "Could not read whether execution {} was also in the log.",
                        execution_i
                    )
                })?;

            //enabled transitions
            let json_enabled = json::read_field_list(&json_execution, JSON_KEY_ENABLED_TRANSITIONS)
                .with_context(|| {
                    anyhow!(
                        "Enabled transitions are missing for execution {}.",
                        execution_i
                    )
                })?;
            let mut enabled_transitions = Vec::with_capacity(json_enabled.len());
            for (enabledd_i, json_enabledd) in json_enabled.into_iter().enumerate() {
                enabled_transitions.push(json::read_index(&json_enabledd).with_context(|| {
                    anyhow!(
                        "Could not read enabled transition {} of execution {}.",
                        enabledd_i,
                        execution_i
                    )
                })?);
            }

            //time of enablement
            let time_of_enablement =
                json::read_field_time_or_null(&json_execution, JSON_KEY_TIME_OF_ENABLEMENT)
                    .with_context(|| {
                        anyhow!(
                            "Could not read time of enablement of execution {}.",
                            execution_i
                        )
                    })?;

            //time of execution
            let time_of_execution =
                json::read_field_time_or_null(&json_execution, JSON_KEY_TIME_OF_EXECUTION)
                    .with_context(|| {
                        anyhow!(
                            "Could not read time of execution of execution {}.",
                            execution_i
                        )
                    })?;

            //resource
            let resource = if let Some(resource) =
                json::read_field_string_or_null(&json_execution, JSON_KEY_RESOURCE).with_context(
                    || anyhow!("Could not read resource of execution {}.", execution_i),
                )? {
                Some(resource_key.process_activity(&resource))
            } else {
                None
            };

            executions.push(Execution {
                activity,
                also_in_log,
                enabled_transitions,
                time_of_enablement,
                time_of_execution,
                resource,
            });
        }

        Ok((activity_key, resource_key, executions).into())
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
        let mut json_executions = Vec::with_capacity(self.executions.len());
        for execution in &self.executions {
            json_executions.push(execution.to_json(&self.activity_key, &self.resource_key));
        }
        let mut json = serde_json::Map::new();
        json.insert(
            JSON_KEY_EXECUTIONS.to_string(),
            serde_json::Value::Array(json_executions),
        );
        let json = Value::Object(json);
        write!(f, "{}", serde_json::to_string_pretty(&json).unwrap())
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

#[derive(Clone, Debug)]
pub struct Execution {
    pub activity: Option<Activity>,
    pub also_in_log: bool,
    /// first enabled transition was actually fired
    pub enabled_transitions: Vec<usize>,
    pub time_of_enablement: Option<DateTime<FixedOffset>>,
    pub time_of_execution: Option<DateTime<FixedOffset>>,
    pub resource: Option<Activity>,
}

impl Execution {
    fn to_json(&self, activity_key: &ActivityKey, resource_key: &ActivityKey) -> Value {
        let mut json_execution = serde_json::Map::new();

        //activity
        json_execution.insert(
            JSON_KEY_ACTIVITY.to_string(),
            if let Some(activity) = self.activity {
                Value::String(activity_key.deprocess_activity(&activity).to_string())
            } else {
                Value::Null
            },
        );

        //also in log
        json_execution.insert(
            JSON_KEY_ALSO_IN_LOG.to_string(),
            Value::Bool(self.also_in_log),
        );

        //enabled transitions
        json_execution.insert(
            JSON_KEY_ENABLED_TRANSITIONS.to_string(),
            Value::Array(
                self.enabled_transitions
                    .iter()
                    .map(|i| Value::Number(Number::from_u128(*i as u128).unwrap()))
                    .collect(),
            ),
        );

        //time of enablement
        json_execution.insert(
            JSON_KEY_TIME_OF_ENABLEMENT.to_string(),
            if let Some(time) = self.time_of_enablement {
                Value::String(time.to_string())
            } else {
                Value::Null
            },
        );

        //time of execution
        json_execution.insert(
            JSON_KEY_TIME_OF_EXECUTION.to_string(),
            if let Some(time) = self.time_of_execution {
                Value::String(time.to_string())
            } else {
                Value::Null
            },
        );

        //resource
        json_execution.insert(
            JSON_KEY_RESOURCE.to_string(),
            if let Some(resource) = self.resource {
                Value::String(resource_key.deprocess_activity(&resource).to_string())
            } else {
                Value::Null
            },
        );

        Value::Object(json_execution)
    }
}

impl Display for Execution {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[cfg(any(test, feature = "testactivities"))]
impl TestActivityKey for Executions {
    fn test_activity_key(&self) {
        self.executions.iter().for_each(|execution| {
            if let Some(activity) = execution.activity {
                self.activity_key().assert_activity_is_of_key(&activity)
            }
        });
    }
}

#[cfg(test)]
mod tests {
    use crate::Executions;
    use std::fs;

    #[test]
    fn executions() {
        let fin = fs::read_to_string("testfiles/a-b.exs").unwrap();
        let exs = fin.parse::<Executions>().unwrap();

        println!("{}", exs);

        assert_eq!(exs.to_string() + "\n", fin);
    }
}
