use anyhow::{Error, anyhow};
use ebi_arithmetic::Fraction;

use crate::{ActivityKey, ebi_objects::{
    process_tree::{Node, ProcessTree}, stochastic_process_tree::StochasticProcessTree,
}};

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
        Ok(Self {
            activity_key: value.0.activity_key,
            tree: value.0.tree,
            transition2node: value.0.transition2node,
            termination_weight: value.2,
            weights: value.1,
        })
    }
}

impl From<(ActivityKey, Vec<Node>, Vec<Fraction>, Fraction)> for StochasticProcessTree {
    fn from(value: (ActivityKey, Vec<Node>, Vec<Fraction>, Fraction)) -> Self {
        let (activity_key, tree, weights, termination_weight) = value;

        let mut transition2node = vec![];
        for (node_index, node) in tree.iter().enumerate() {
            match node {
                Node::Tau | Node::Activity(_) => {
                    transition2node.push(node_index);
                }
                Node::Operator(_, _) => {}
            }
        }

        Self {
            activity_key: activity_key,
            tree: tree,
            transition2node: transition2node,
            weights: weights,
            termination_weight: termination_weight,
        }
    }
}
