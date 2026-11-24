use crate::{
    ActivityKey,
    ebi_objects::{
        process_tree::{ProcessTree, TreeNode},
        stochastic_process_tree::StochasticProcessTree,
    },
};
use ebi_bpmn::ebi_arithmetic::{
    Fraction,
    anyhow::{Error, anyhow},
};

impl TryFrom<(ProcessTree, Vec<Fraction>, Fraction)> for StochasticProcessTree {
    type Error = Error;
    fn try_from(value: (ProcessTree, Vec<Fraction>, Fraction)) -> Result<Self, Self::Error> {
        if value.0.number_of_leaves() != value.1.len() {
            return Err(anyhow!(
                "non-appropriate number of weights ({}): must be equal to the number of leaves ({})",
                value.0.number_of_leaves(),
                value.1.len()
            ));
        }
        let (activity_key, tree) = value.0.take();
        Ok(Self::new(activity_key, tree, value.1, value.2))
    }
}

impl From<(ActivityKey, Vec<TreeNode>, Vec<Fraction>, Fraction)> for StochasticProcessTree {
    fn from(value: (ActivityKey, Vec<TreeNode>, Vec<Fraction>, Fraction)) -> Self {
        let (activity_key, tree, weights, termination_weight) = value;
        Self::new(activity_key, tree, weights, termination_weight)
    }
}
