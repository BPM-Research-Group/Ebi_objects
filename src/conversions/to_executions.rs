use ebi_activity_key::ActivityKey;

use crate::{AttributeKey, Executions, ebi_objects::executions::Execution};

impl From<(ActivityKey, ActivityKey, AttributeKey, Vec<Execution>)> for Executions {
    fn from(value: (ActivityKey, ActivityKey, AttributeKey, Vec<Execution>)) -> Self {
        Self {
            activity_key: value.0,
            resource_key: value.1,
            attribute_key: value.2,
            executions: value.3,
        }
    }
}
