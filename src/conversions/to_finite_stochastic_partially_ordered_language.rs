use crate::{
    FiniteStochasticPartiallyOrderedLanguage,
    ebi_objects::finite_stochastic_partially_ordered_language::PartiallyOrderedTrace,
};
use bitvec::{bitvec, prelude::Lsb0};
use ebi_activity_key::{Activity, ActivityKey};
use ebi_arithmetic::Fraction;
use ebi_bpmn::partially_ordered_run::PartiallyOrderedRun;
use std::{collections::BTreeSet, usize};

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
        let edge_sequence = edge_execution_sequence(&value);

        let PartiallyOrderedRun {
            edge_2_inputs,
            edge_2_activity,
            state_2_output_edge,
            ..
        } = value;

        //create the nodes
        let mut edge_2_node = vec![0; edge_2_inputs.len()];
        let node_2_activity = edge_sequence
            .iter()
            .enumerate()
            .map(|(node, edge)| {
                edge_2_node[*edge] = node;
                edge_2_activity[*edge]
            })
            .collect();

        let mut node_2_predecessors = vec![vec![]; edge_2_inputs.len()];
        for (edge, input_states) in edge_2_inputs.iter().enumerate() {
            for input_state in input_states {
                if let Some(predecessor_edge) = state_2_output_edge[*input_state] {
                    node_2_predecessors[edge_2_node[edge]].push(edge_2_node[predecessor_edge]);
                }
            }
        }

        Self {
            node_2_activity,
            node_2_predecessors,
        }
    }
}

fn edge_execution_sequence(run: &PartiallyOrderedRun) -> Vec<usize> {
    let mut state_states = bitvec![0; run.number_of_states()];
    for state in 0..run.number_of_states() {
        if run.state_2_input_edge[state].is_none() {
            state_states.set(state, true);
        }
    }
    let mut state_edges = bitvec![0; run.number_of_states()];
    let mut result = vec![];
    while result.len() != run.number_of_edges() {
        'outer: for edge in 0..run.number_of_edges() {
            if !state_edges[edge] {
                for input in &run.edge_2_inputs[edge] {
                    if !state_states[*input] {
                        continue 'outer;
                    }
                }
                //execute edge
                result.push(edge);
                for output in &run.edge_2_outputs[edge] {
                    state_states.set(*output, true);
                }
                state_edges.set(edge, true);
            }
        }
    }
    result
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

#[cfg(test)]
mod tests {
    use std::fs;

    use crate::{
        conversions::to_finite_stochastic_partially_ordered_language::PartiallyOrderedLabelledTrace,
        ebi_objects::finite_stochastic_partially_ordered_language::PartiallyOrderedTrace,
    };
    use ebi_activity_key::ActivityKey;
    use ebi_bpmn::{
        StochasticBusinessProcessModelAndNotation, partially_ordered_run::PartiallyOrderedRun,
    };

    #[test]
    fn poltrace_2_potrace_empty() {
        let ltrace = PartiallyOrderedLabelledTrace {
            node_2_activity: vec![],
            node_2_predecessors: vec![],
        };

        let trace = PartiallyOrderedTrace {
            node_2_activity: vec![],
            node_2_predecessors: vec![],
        };

        assert_eq!(PartiallyOrderedTrace::from(ltrace), trace);
    }

    #[test]
    fn poltrace_2_potrace() {
        let mut activity_key = ActivityKey::new();
        let abc = activity_key.process_activity("abc");
        let def = activity_key.process_activity("def");

        let ltrace = PartiallyOrderedLabelledTrace {
            node_2_activity: vec![Some(abc), None, Some(def)],
            node_2_predecessors: vec![vec![], vec![0], vec![1]],
        };

        let trace = PartiallyOrderedTrace {
            node_2_activity: vec![abc, def],
            node_2_predecessors: vec![vec![], vec![0]],
        };

        assert_eq!(PartiallyOrderedTrace::from(ltrace), trace);
    }

    #[test]
    fn porun_2_potrace() {
        let fin = fs::read_to_string("testfiles/flower.sbpmn").unwrap();
        let sbpmn = fin
            .parse::<StochasticBusinessProcessModelAndNotation>()
            .unwrap();

        let run = PartiallyOrderedRun::new_random(&sbpmn).unwrap();
        let _ = PartiallyOrderedTrace::from(run);
    }
}
