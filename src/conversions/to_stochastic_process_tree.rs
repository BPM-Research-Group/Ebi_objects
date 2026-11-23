use crate::{
    ActivityKey,
    ebi_objects::{
        process_tree::{Node, ProcessTree},
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
        Ok(Self {
            activity_key: value.0.activity_key,
            tree: value.0.tree,
            cache_transition_2_node: value.0.cache_transition_2_node,
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
            activity_key,
            tree,
            cache_transition_2_node: transition2node,
            weights,
            termination_weight,
        }
    }
}
