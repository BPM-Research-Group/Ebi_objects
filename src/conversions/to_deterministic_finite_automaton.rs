use std::collections::{BTreeSet, HashMap, HashSet, VecDeque};

use crate::{
    ActivityKey, ActivityKeyTranslator, CompressedEventLog, CompressedEventLogXes, EventLogCsv,
    EventLogTraceAttributes, EventLogXes, NumberOfTraces, ProcessTree, ProcessTreeMarkupLanguage,
    StochasticNondeterministicFiniteAutomaton, StochasticProcessTree,
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

impl From<FiniteStochasticLanguage> for DeterministicFiniteAutomaton {
    fn from(value: FiniteStochasticLanguage) -> Self {
        Into::<FiniteLanguage>::into(value).into()
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
        DeterministicFiniteAutomaton, ProcessTree, StochasticNondeterministicFiniteAutomaton,
        StochasticProcessTree,
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
}
