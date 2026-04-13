use ebi_activity_key::ActivityKey;

use crate::{Executions, ebi_objects::executions::Execution};

impl From<(ActivityKey, ActivityKey, Vec<Execution>)> for Executions {
    fn from(value: (ActivityKey, ActivityKey, Vec<Execution>)) -> Self {
        Self {
            activity_key: value.0,
            resource_key: value.1,
            executions: value.2,
        }
    }
}
