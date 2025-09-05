use ebi_arithmetic::Fraction;

use crate::ebi_objects::{process_tree::ProcessTree, stochastic_process_tree::StochasticProcessTree};

impl From<(ProcessTree, Vec<Fraction>, Fraction)> for StochasticProcessTree {
    fn from(value: (ProcessTree, Vec<Fraction>, Fraction)) -> Self {
        assert_eq!(value.0.get_number_of_nodes() - 1, value.1.len());
        Self {
            activity_key: value.0.activity_key,
            tree: value.0.tree,
            transition2node: value.0.transition2node,
            termination_weight: value.2,
            weights: value.1,
        }
    }
}