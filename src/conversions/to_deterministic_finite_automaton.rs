use crate::{
    ActivityKey, ActivityKeyTranslator, CompressedEventLog, CompressedEventLogXes,
    DirectlyFollowsGraph, DirectlyFollowsModel, EventLogCsv, EventLogOcel, EventLogTraceAttributes,
    EventLogXes, NumberOfTraces, ProcessTree, ProcessTreeMarkupLanguage,
    StochasticDirectlyFollowsModel, StochasticNondeterministicFiniteAutomaton,
    StochasticProcessTree,
    activity_key::has_activity_key::HasActivityKey,
    ebi_objects::{
        compressed_event_log_trace_attributes::CompressedEventLogTraceAttributes,
        deterministic_finite_automaton::DeterministicFiniteAutomaton,
        event_log::EventLog,
        event_log_python::EventLogPython,
        finite_language::FiniteLanguage,
        finite_stochastic_language::FiniteStochasticLanguage,
        process_tree::{
            execute_transition, get_enabled_transitions, get_initial_state,
            get_transition_activity, is_final_state,
        },
        stochastic_deterministic_finite_automaton::StochasticDeterministicFiniteAutomaton,
    },
};
use ebi_arithmetic::ebi_number::{Signed, Zero};
use std::collections::{BTreeSet, HashMap, HashSet, VecDeque};

impl From<FiniteLanguage> for DeterministicFiniteAutomaton {
    fn from(value: FiniteLanguage) -> Self {
        log::info!("convert finite language into a DFA");
        let mut result = DeterministicFiniteAutomaton::new();
        result.set_activity_key(value.activity_key().clone());

        if value.number_of_traces().is_zero() {
            result.set_initial_state(None);
        } else {
            for trace in value.traces.iter() {
                let mut state = result.initial_state.unwrap();

                for activity in trace {
                    state = result.take_or_add_transition(state, *activity);
                }

                result.set_final_state(state, true);
            }
        }

        result
    }
}

impl From<StochasticDeterministicFiniteAutomaton> for DeterministicFiniteAutomaton {
    fn from(value: StochasticDeterministicFiniteAutomaton) -> Self {
        log::info!("convert SDFA into DFA");
        let final_states = value
            .terminating_probabilities
            .iter()
            .map(|p| p.is_positive())
            .collect();

        Self {
            activity_key: value.activity_key,
            initial_state: value.initial_state,
            sources: value.sources,
            targets: value.targets,
            activities: value.activities,
            final_states: final_states,
        }
    }
}

impl From<DirectlyFollowsModel> for DeterministicFiniteAutomaton {
    fn from(value: DirectlyFollowsModel) -> Self {
        log::info!("convert DFM into DFA");

        if value.node_2_activity.is_empty() && !value.has_empty_traces() {
            //empty language
            return Self {
                activity_key: ActivityKey::new(),
                initial_state: None,
                sources: vec![],
                targets: vec![],
                activities: vec![],
                final_states: vec![],
            };
        }

        let number_of_nodes = value.node_2_activity.len();

        //copy the edges, as they did not change; just make the labels explicit
        let DirectlyFollowsModel {
            sources,
            targets,
            node_2_activity,
            empty_traces,
            start_nodes,
            end_nodes,
            ..
        } = value;
        let activities = targets
            .iter()
            .map(|target| node_2_activity[*target])
            .collect();

        //prepare final states: initial state is final if empty traces are present
        let mut final_states = end_nodes;
        final_states.push(empty_traces);

        //construct result
        let mut result = Self {
            activity_key: value.activity_key,
            initial_state: Some(number_of_nodes),
            sources,
            targets,
            activities,
            final_states,
        };

        //add the start activities
        for start_node in 0..number_of_nodes {
            if start_nodes[start_node] {
                result
                    .add_transition(number_of_nodes, node_2_activity[start_node], start_node)
                    .unwrap(); //by construction, determinism is guaranteed
            }
        }

        result
    }
}

impl From<StochasticDirectlyFollowsModel> for DeterministicFiniteAutomaton {
    fn from(value: StochasticDirectlyFollowsModel) -> Self {
        Into::<DirectlyFollowsModel>::into(value).into()
    }
}

impl From<FiniteStochasticLanguage> for DeterministicFiniteAutomaton {
    fn from(value: FiniteStochasticLanguage) -> Self {
        Into::<FiniteLanguage>::into(value).into()
    }
}

impl From<DirectlyFollowsGraph> for DeterministicFiniteAutomaton {
    fn from(value: DirectlyFollowsGraph) -> Self {
        StochasticNondeterministicFiniteAutomaton::from(value).into()
    }
}

macro_rules! via_lang {
    ($t:ident) => {
        impl From<$t> for DeterministicFiniteAutomaton {
            fn from(value: $t) -> Self {
                Into::<FiniteLanguage>::into(value).into()
            }
        }
    };
}

via_lang!(EventLog);
via_lang!(EventLogTraceAttributes);
via_lang!(EventLogCsv);
via_lang!(EventLogOcel);
via_lang!(EventLogXes);
via_lang!(EventLogPython);
via_lang!(CompressedEventLog);
via_lang!(CompressedEventLogTraceAttributes);
via_lang!(CompressedEventLogXes);

impl From<ProcessTree> for DeterministicFiniteAutomaton {
    fn from(tree: ProcessTree) -> Self {
        log::info!("convert process tree to DFA");

        let initial_marking = if let Some(marking) = get_initial_state(&tree) {
            marking
        } else {
            //empty language
            return Self {
                activity_key: ActivityKey::new(),
                initial_state: None,
                sources: vec![],
                targets: vec![],
                activities: vec![],
                final_states: vec![],
            };
        };

        let mut result = DeterministicFiniteAutomaton::new();
        let translator = ActivityKeyTranslator::new(tree.activity_key(), result.activity_key_mut());

        let mut markingset2node = HashMap::new();

        //initial state
        let source_node = result.initial_state.unwrap(); //exists by contract of new()
        let initial_markingset = BTreeSet::from([initial_marking]);
        markingset2node.insert(initial_markingset.clone(), source_node);
        let mut queue = VecDeque::new();
        queue.push_back(initial_markingset);

        while let Some(markingset) = queue.pop_front() {
            let node = markingset2node[&markingset];

            //gather activity -> marking set (for each activity, the set of markings that can be reached with that activity)
            let mut activity2markings = HashMap::new();
            {
                let mut inner_queue = VecDeque::new();
                let mut inner_visited = HashSet::new();
                inner_queue.extend(markingset.clone());
                inner_visited.extend(markingset);

                //walk through all markings
                while let Some(marking) = inner_queue.pop_front() {
                    if is_final_state(&tree, &marking) {
                        result.set_final_state(node, true);
                    } else {
                        for transition in get_enabled_transitions(&tree, &marking) {
                            let mut target_marking = marking.clone();
                            execute_transition(&tree, &mut target_marking, transition).unwrap(); //by construction, transition is enabled

                            if let Some(activity) = get_transition_activity(&tree, transition) {
                                //labelled activity: insert into resulting map
                                activity2markings
                                    .entry(translator.translate_activity(&activity))
                                    .or_insert_with(|| BTreeSet::new())
                                    .insert(target_marking);
                            } else {
                                //silent step: add to queue
                                if inner_visited.insert(target_marking.clone()) {
                                    inner_queue.push_back(target_marking);
                                }
                            }
                        }
                    }
                }
            }

            //add the activity -> marking set mapping to the automaton
            for (activity, markingset) in activity2markings {
                let target_node = markingset2node
                    .entry(markingset.clone())
                    .or_insert_with(|| {
                        queue.push_back(markingset);
                        result.add_state()
                    });

                result.add_transition(node, activity, *target_node).unwrap(); //by construction, this should not fail.
            }
        }

        result
    }
}

macro_rules! via_tree {
    ($t:ident) => {
        impl From<$t> for DeterministicFiniteAutomaton {
            fn from(value: $t) -> Self {
                let tree = ProcessTree::from(value);
                tree.into()
            }
        }
    };
}

via_tree!(StochasticProcessTree);
via_tree!(ProcessTreeMarkupLanguage);

impl From<StochasticNondeterministicFiniteAutomaton> for DeterministicFiniteAutomaton {
    fn from(snfa: StochasticNondeterministicFiniteAutomaton) -> Self {
        log::info!("convert SNFA to DFA");

        let initial_marking = if let Some(marking) = snfa.initial_state {
            marking
        } else {
            //empty language
            return Self {
                activity_key: ActivityKey::new(),
                initial_state: None,
                sources: vec![],
                targets: vec![],
                activities: vec![],
                final_states: vec![],
            };
        };

        let mut result = DeterministicFiniteAutomaton::new();
        let translator = ActivityKeyTranslator::new(snfa.activity_key(), result.activity_key_mut());

        let mut markingset2node = HashMap::new();

        //initial state
        let source_node = result.initial_state.unwrap(); //exists by contract of new()
        let initial_markingset = BTreeSet::from([initial_marking]);
        markingset2node.insert(initial_markingset.clone(), source_node);
        let mut queue = VecDeque::new();
        queue.push_back(initial_markingset);

        while let Some(markingset) = queue.pop_front() {
            let node = markingset2node[&markingset];

            //gather activity -> marking set (for each activity, the set of markings that can be reached with that activity)
            let mut activity2markings = HashMap::new();
            {
                let mut inner_queue = VecDeque::new();
                let mut inner_visited = HashSet::new();
                inner_queue.extend(markingset.clone());
                inner_visited.extend(markingset);

                //walk through all markings
                while let Some(marking) = inner_queue.pop_front() {
                    if snfa.get_termination_probability(marking).is_positive() {
                        result.set_final_state(node, true);
                    } else {
                        for (_, target_marking, label, _) in snfa.outgoing_edges(marking) {
                            if let Some(activity) = label {
                                //labelled activity: insert into resulting map
                                activity2markings
                                    .entry(translator.translate_activity(&activity))
                                    .or_insert_with(|| BTreeSet::new())
                                    .insert(*target_marking);
                            } else {
                                //silent step: add to queue
                                if inner_visited.insert(target_marking.clone()) {
                                    inner_queue.push_back(*target_marking);
                                }
                            }
                        }
                    }
                }
            }

            //add the activity -> marking set mapping to the automaton
            for (activity, markingset) in activity2markings {
                let target_node = markingset2node
                    .entry(markingset.clone())
                    .or_insert_with(|| {
                        queue.push_back(markingset);
                        result.add_state()
                    });

                result.add_transition(node, activity, *target_node).unwrap(); //by construction, this should not fail.
            }
        }

        result
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        DeterministicFiniteAutomaton, ProcessTree, StochasticDirectlyFollowsModel,
        StochasticNondeterministicFiniteAutomaton, StochasticProcessTree,
    };
    use std::fs;

    #[test]
    fn tree_to_dfa() {
        let fin = fs::read_to_string("testfiles/seq(a-xor(b-c)).sptree").unwrap();
        let tree: ProcessTree = fin.parse::<StochasticProcessTree>().unwrap().into();

        let dfa = DeterministicFiniteAutomaton::from(tree);
        assert_eq!(
            format!("{}", dfa),
            "{
\"initialState\": 0,
\"transitions\": [
{\"from\":0,\"to\":1,\"label\":\"a\"},
{\"from\":1,\"to\":2,\"label\":\"b\"},
{\"from\":1,\"to\":2,\"label\":\"c\"}
], \"finalStates\": [
2
]}
"
        );
    }

    #[test]
    fn snfa_to_dfa() {
        let fin = fs::read_to_string("testfiles/aa-ab-ba.snfa").unwrap();
        let snfa = fin
            .parse::<StochasticNondeterministicFiniteAutomaton>()
            .unwrap();

        let dfa = DeterministicFiniteAutomaton::from(snfa);

        assert_eq!(dfa.number_of_states(), 4);
        assert_eq!(dfa.get_sources().len(), 5);
    }

    #[test]
    fn dfm_to_dfa() {
        let fin = fs::read_to_string("testfiles/aa-ab-ba.sdfm").unwrap();
        let sdfm = fin.parse::<StochasticDirectlyFollowsModel>().unwrap();

        let dfa = DeterministicFiniteAutomaton::from(sdfm);

        assert_eq!(dfa.number_of_states(), 3);
        assert_eq!(dfa.sources.len(), 5);
    }
}
