use anyhow::{Error, anyhow};
use ebi_arithmetic::Fraction;

use crate::ebi_objects::{
    process_tree::ProcessTree, stochastic_process_tree::StochasticProcessTree,
};

impl TryFrom<(ProcessTree, Vec<Fraction>, Fraction)> for StochasticProcessTree {
    type Error = Error;
    fn try_from(value: (ProcessTree, Vec<Fraction>, Fraction)) -> Result<Self, Self::Error> {
        if value.0.number_of_leaves() != value.1.len() {
            return Err(anyhow!(
                "non-appropriate number of weights: must be equal to the number of leaves"
            ));
        }
        Ok(Self {
            activity_key: value.0.activity_key,
            tree: value.0.tree,
            transition2node: value.0.transition2node,
            termination_weight: value.2,
            weights: value.1,
        })
    }
}
