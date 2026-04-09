use std::{collections::BTreeSet, usize};

use crate::{
    FiniteStochasticPartiallyOrderedLanguage,
    ebi_objects::finite_stochastic_partially_ordered_language::PartiallyOrderedTrace,
};
use ebi_activity_key::{Activity, ActivityKey};
use ebi_arithmetic::Fraction;
use ebi_bpmn::partially_ordered_run::PartiallyOrderedRun;

impl From<(ActivityKey, Vec<PartiallyOrderedTrace>, Vec<Fraction>)>
    for FiniteStochasticPartiallyOrderedLanguage
{
    fn from(value: (ActivityKey, Vec<PartiallyOrderedTrace>, Vec<Fraction>)) -> Self {
        let (activity_key, traces, probabilities) = value;
        Self {
            activity_key,
            traces,
            probabilities,
        }
    }
}

struct PartiallyOrderedLabelledTrace {
    pub node_2_activity: Vec<Option<Activity>>,
    pub node_2_predecessors: Vec<Vec<usize>>,
}

impl From<PartiallyOrderedRun> for PartiallyOrderedLabelledTrace {
    fn from(value: PartiallyOrderedRun) -> Self {
        let PartiallyOrderedRun {
            edge_2_inputs,
            edge_2_outputs,
            edge_2_activity,
            ..
        } = value;
    }
}

impl From<PartiallyOrderedLabelledTrace> for PartiallyOrderedTrace {
    fn from(value: PartiallyOrderedLabelledTrace) -> Self {
        let PartiallyOrderedLabelledTrace {
            node_2_activity,
            node_2_predecessors,
        } = value;
        //create a map node -> new node
        let mut node_2_new_node = Vec::with_capacity(node_2_activity.len());
        let mut new_node_2_activity = vec![];
        {
            let mut removed_nodes = 0;
            for (node, activity) in node_2_activity.into_iter().enumerate() {
                if let Some(activity) = activity {
                    node_2_new_node.push(Some(node - removed_nodes));
                    new_node_2_activity.push(activity);
                } else {
                    removed_nodes += 1;
                    node_2_new_node.push(None);
                }
            }
        }

        //reroute the edges
        let mut new_node_2_predecessors = Vec::with_capacity(new_node_2_activity.len());
        for (node, predecessors) in node_2_predecessors.iter().enumerate() {
            if node_2_new_node[node].is_some() {
                let mut new_predecessors: BTreeSet<usize> = BTreeSet::new();
                let mut queue: Vec<usize> = vec![];
                queue.extend(predecessors);
                while let Some(predecessor) = queue.pop() {
                    if let Some(new_predecessor) = node_2_new_node[predecessor] {
                        new_predecessors.insert(new_predecessor);
                    } else {
                        //predecessor is going to be removed; keep walking back
                        queue.extend(&node_2_predecessors[predecessor]);
                    }
                }
                new_node_2_predecessors.push(new_predecessors.into_iter().collect());
            }
        }

        Self {
            node_2_activity: new_node_2_activity,
            node_2_predecessors: new_node_2_predecessors,
        }
    }
}

impl From<PartiallyOrderedRun> for PartiallyOrderedTrace {
    fn from(value: PartiallyOrderedRun) -> Self {
        let labelled_trace = PartiallyOrderedLabelledTrace::from(value);
        labelled_trace.into()
    }
}
