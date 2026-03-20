use std::collections::HashMap;

use ebi_activity_key::ActivityKey;
use ebi_arithmetic::Fraction;
use ebi_bpmn::partially_ordered_run::PartiallyOrderedRun;

use crate::{
    FiniteStochasticPartiallyOrderedLanguage,
    ebi_objects::finite_stochastic_partially_ordered_language::PartiallyOrderedTrace,
};

impl From<PartiallyOrderedRun> for PartiallyOrderedTrace {
    fn from(value: PartiallyOrderedRun) -> Self {
        let number_of_states = value.number_of_states();
        let PartiallyOrderedRun {
            edge_2_inputs,
            edge_2_outputs,
            edge_2_activity,
            ..
        } = value;
        Self {
            number_of_states,
            edge_2_inputs,
            edge_2_outputs,
            edge_2_activity,
        }
    }
}

impl From<(ActivityKey, HashMap<PartiallyOrderedTrace, Fraction>)>
    for FiniteStochasticPartiallyOrderedLanguage
{
    fn from(value: (ActivityKey, HashMap<PartiallyOrderedTrace, Fraction>)) -> Self {
        let (activity_key, traces) = value;
        Self {
            activity_key,
            traces,
        }
    }
}
