use crate::{
    ActivityKey, ActivityKeyTranslator, AutomatonState, CompressedEventLog,
    CompressedEventLogTraceAttributes, CompressedEventLogXes, DirectlyFollowsGraph, EventLog,
    EventLogCsv, EventLogOcel, EventLogPython, EventLogTraceAttributes, EventLogXes,
    FiniteStochasticLanguage, HasActivityKey, NumberOfTraces,
    StochasticDeterministicFiniteAutomaton, StochasticDirectlyFollowsModel,
    StochasticNondeterministicFiniteAutomaton, StochasticProcessTree,
    ebi_objects::stochastic_process_tree::{
        execute_transition, get_enabled_transitions, get_initial_state,
        get_total_weight_of_enabled_transitions, get_transition_activity, get_transition_weight,
    },
};
use ebi_bpmn::ebi_arithmetic::{Fraction, Signed, Zero};
use std::collections::{HashMap, VecDeque, hash_map::Entry};

impl From<FiniteStochasticLanguage> for StochasticNondeterministicFiniteAutomaton {
    fn from(value: FiniteStochasticLanguage) -> Self {
        log::info!("convert finite stochastic language to SNFA");

        let mut result = StochasticNondeterministicFiniteAutomaton::new();
        if value.number_of_traces().is_zero() {
            result.initial_state = None;
        } else {
            let mut final_states = HashMap::new();
            result.activity_key = value.activity_key;

            //create automaton
            for (trace, probability) in &value.traces {
                let mut state = result.initial_state.unwrap();
                for activity in trace {
                    state =
                        result.take_or_add_transition(state, Some(*activity), probability.clone());
                }

                match final_states.entry(state) {
                    Entry::Occupied(mut e) => *e.get_mut() += probability,
                    Entry::Vacant(e) => {
                        e.insert(probability.clone());
                    }
                }
            }

            //count
            let mut sums = final_states;
            for (source, _, _, probability) in &result {
                match sums.entry(*source) {
                    Entry::Occupied(mut e) => *e.get_mut() += probability,
                    Entry::Vacant(e) => {
                        e.insert(probability.clone());
                    }
                }
            }

            //normalise
            result.scale_outgoing_probabilities(sums);
        }
        result
    }
}

macro_rules! from_via_slang {
    ($t:ident) => {
        impl From<$t> for StochasticNondeterministicFiniteAutomaton {
            fn from(value: $t) -> Self {
                FiniteStochasticLanguage::from(value).into()
            }
        }
    };
}

from_via_slang!(EventLog);
from_via_slang!(EventLogTraceAttributes);
from_via_slang!(EventLogXes);
from_via_slang!(EventLogCsv);
from_via_slang!(EventLogOcel);
from_via_slang!(EventLogPython);
from_via_slang!(CompressedEventLogXes);
from_via_slang!(CompressedEventLog);
from_via_slang!(CompressedEventLogTraceAttributes);

impl From<StochasticDeterministicFiniteAutomaton> for StochasticNondeterministicFiniteAutomaton {
    fn from(value: StochasticDeterministicFiniteAutomaton) -> Self {
        let StochasticDeterministicFiniteAutomaton {
            activities,
            activity_key,
            initial_state,
            probabilities,
            sources,
            targets,
            terminating_probabilities,
            ..
        } = value;

        Self {
            activity_key,
            initial_state,
            sources,
            targets,
            activities: activities.into_iter().map(|a| Some(a)).collect(),
            probabilities,
            termination_probabilities: terminating_probabilities,
        }
    }
}

impl From<DirectlyFollowsGraph> for StochasticNondeterministicFiniteAutomaton {
    fn from(value: DirectlyFollowsGraph) -> Self {
        let mut result = Self::new();
        let initial_state = 0;

        //add states
        for _ in value.activity_key().get_activities() {
            result.add_state(); //we do not keep a map, as we can predict the numbers: 0 = initial state, 1..=n = activity states, n+1 = final state.
        }

        //start activities
        {
            let mut sum_of_start_activities = value
                .start_activities
                .iter()
                .map(|(_, f)| f)
                .sum::<Fraction>();
            sum_of_start_activities += &value.empty_traces_weight;
            for (node, weight) in &value.start_activities {
                let activity = value.state_2_activity[node.0];
                result
                    .add_transition(
                        AutomatonState::of(initial_state),
                        Some(activity),
                        AutomatonState::of(activity.id + 1),
                        weight / &sum_of_start_activities,
                    )
                    .unwrap(); //by construction, outgoing probability cannot become lower than 0
            }

            //empty traces are implied
        }

        //edges
        {
            for activity in value.activity_key().get_activities() {
                if let Some(node) = value.activity_2_state.get(*activity) {
                    //gather the outgoing sum
                    let mut sum = Fraction::zero();
                    {
                        let (_, mut i) = value.binary_search(*node, AutomatonState::zero());
                        while i < value.sources.len() && &value.sources[i] == node {
                            if value.weights[i].is_positive() {
                                sum += &value.weights[i];
                            }
                            i += 1;
                        }

                        if let Some(w) = value.end_activities.get(*node)
                            && w.is_positive()
                        {
                            sum += w;
                        }
                    }

                    // add the edges
                    let (_, mut i) = value.binary_search(*node, AutomatonState::zero());
                    while i < value.sources.len() && &value.sources[i] == node {
                        if value.weights[i].is_positive() {
                            result
                                .add_transition(
                                    AutomatonState::of(activity.id + 1),
                                    Some(value.state_2_activity[value.targets[i].0]),
                                    AutomatonState::of(
                                        value.state_2_activity[value.targets[i].0].id + 1,
                                    ),
                                    &value.weights[i] / &sum,
                                )
                                .unwrap(); //by construction, remaining outgoing probability cannot become negative
                        }
                        i += 1;
                    }
                    // termination is implied
                }
            }
        }

        result.activity_key = value.activity_key;

        result
    }
}

impl From<StochasticDirectlyFollowsModel> for StochasticNondeterministicFiniteAutomaton {
    fn from(value: StochasticDirectlyFollowsModel) -> Self {
        let mut result = Self::new();

        if value.node_2_activity.is_empty() && !value.empty_traces_weight.is_positive() {
            //empty language
            result.initial_state = None;
            return result;
        }

        let initial_state = AutomatonState::zero();
        result.initial_state = Some(initial_state);

        //add states
        for _ in 0..value.start_node_weights.len() {
            result.add_state(); //we do not keep a map, as we can predict the numbers: 0 = initial state, 1..=n = activity states.
        }

        //start activities
        {
            let mut sum_of_start_activities: Fraction = value.start_node_weights.iter().sum();
            sum_of_start_activities += &value.empty_traces_weight;
            for (node, weight) in value.start_node_weights.iter().enumerate() {
                if weight.is_positive() {
                    result
                        .add_transition(
                            initial_state,
                            Some(value.node_2_activity[node]),
                            AutomatonState::of(node + 1),
                            weight / &sum_of_start_activities,
                        )
                        .unwrap(); //by construction, outgoing probability cannot become lower than 0
                }
            }

            //empty traces are captured by termination probability of initial state
        }

        //edges
        {
            for source in 0..value.start_node_weights.len() {
                //gather the outgoing sum
                let mut sum = Fraction::zero();
                {
                    let (_, mut i) =
                        value.binary_search(AutomatonState::of(source), AutomatonState::zero());
                    while i < value.sources.len() && value.sources[i] == AutomatonState::of(source)
                    {
                        if value.weights[i].is_positive() {
                            sum += &value.weights[i];
                        }
                        i += 1;
                    }

                    if value.end_node_weights[source].is_positive() {
                        sum += &value.end_node_weights[source];
                    }
                }

                // add the edges
                let (_, mut i) =
                    value.binary_search(AutomatonState::of(source), AutomatonState::zero());
                while i < value.sources.len() && value.sources[i] == AutomatonState::of(source) {
                    if value.weights[i].is_positive() {
                        result
                            .add_transition(
                                AutomatonState::of(source + 1),
                                Some(value.node_2_activity[value.targets[i]]),
                                AutomatonState::of(value.targets[i].0 + 1),
                                &value.weights[i] / &sum,
                            )
                            .unwrap(); //by construction, remaining outgoing probability cannot become negative
                    }
                    i += 1;
                }

                // termination is implied
            }
        }

        result.activity_key = value.activity_key;

        result
    }
}

impl From<StochasticProcessTree> for StochasticNondeterministicFiniteAutomaton {
    fn from(tree: StochasticProcessTree) -> Self {
        log::info!("convert (stochastic) process tree to SNFA");

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
                probabilities: vec![],
                termination_probabilities: vec![],
            };
        };

        let mut result = StochasticNondeterministicFiniteAutomaton::new();
        let translator = ActivityKeyTranslator::new(tree.activity_key(), result.activity_key_mut());

        let mut marking2node = HashMap::new();

        //initial state
        let source_node = result.initial_state.unwrap(); //exists by contract of new()
        marking2node.insert(initial_marking.clone(), source_node);

        let mut queue = VecDeque::new();
        queue.push_back(initial_marking);

        while let Some(marking) = queue.pop_front() {
            let node = marking2node[&marking];

            let sum_weight = get_total_weight_of_enabled_transitions(&tree, &marking);
            for transition in get_enabled_transitions(&tree, &marking) {
                let mut target_marking = marking.clone();
                execute_transition(&tree, &mut target_marking, transition).unwrap(); //by construction, transition is enabled

                //get the target node
                let target_node = marking2node
                    .entry(target_marking.clone())
                    .or_insert_with(|| {
                        queue.push_back(target_marking.clone());
                        result.add_state()
                    });

                //add the transition
                let probability =
                    get_transition_weight(&tree, &marking, transition).unwrap() / &sum_weight;
                if let Some(activity) = get_transition_activity(&tree, transition) {
                    //labelled transition
                    let new_activity = translator.translate_activity(&activity);
                    result
                        .add_transition(node, Some(new_activity), *target_node, probability)
                        .unwrap(); //by construction, sum outgoing weight cannot surpass 1
                } else {
                    //silent transition
                    result
                        .add_transition(node, None, *target_node, probability)
                        .unwrap(); //by construction, sum outgoing weight cannot surpass 1
                }
            }
        }

        result
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        DirectlyFollowsGraph, FiniteStochasticLanguage, HasActivityKey,
        StochasticDirectlyFollowsModel, StochasticNondeterministicFiniteAutomaton,
        StochasticProcessTree, a,
    };
    use ebi_bpmn::ebi_arithmetic::{Fraction, One, Zero, f0, f1};
    use std::fs;

    #[test]
    fn dfg_to_snfa() {
        let fin1 = fs::read_to_string("testfiles/aa-ab-ba.dfg").unwrap();
        let mut dfg = fin1.parse::<DirectlyFollowsGraph>().unwrap();

        let a = dfg.activity_key_mut().process_activity("a");
        let b = dfg.activity_key_mut().process_activity("b");
        assert_eq!(dfg.start_activity_weight(a), Fraction::from((2, 5)));
        assert_eq!(dfg.start_activity_weight(b), Fraction::from((3, 5)));

        let snfa: StochasticNondeterministicFiniteAutomaton = dfg.into();
        assert_eq!(snfa.termination_probabilities.len(), 3);
        assert_eq!(snfa.sources.len(), 5);
        assert_eq!(snfa.sources, [a!(0), a!(0), a!(1), a!(1), a!(2)]);
        assert_eq!(snfa.targets, [a!(1), a!(2), a!(1), a!(2), a!(1)]);
        assert_eq!(
            snfa.probabilities,
            [
                Fraction::from((2, 5)),
                Fraction::from((3, 5)),
                Fraction::from((1, 6)),
                Fraction::from((1, 6)),
                Fraction::from((3, 4))
            ]
        );
        assert_eq!(
            snfa.termination_probabilities,
            [f0!(), Fraction::from((2, 3)), Fraction::from((1, 4))]
        );
    }

    #[test]
    fn log_to_sdfa() {
        let fin1 = fs::read_to_string("testfiles/acb-abc-ad-aded-adeded-adededed.slang").unwrap();
        let slang = fin1.parse::<FiniteStochasticLanguage>().unwrap();

        let sdfa = StochasticNondeterministicFiniteAutomaton::from(slang);

        assert!(
            sdfa.termination_probabilities
                .contains(&Fraction::from((8, 15)))
        );
        assert!(
            sdfa.termination_probabilities
                .contains(&Fraction::from((2, 3)))
        );
        assert!(
            sdfa.termination_probabilities
                .contains(&Fraction::from((4, 7)))
        );
    }

    #[test]
    fn sptree_to_snfa() {
        let fin = fs::read_to_string("testfiles/seq(a-xor(b-c)).sptree").unwrap();
        let tree = fin.parse::<StochasticProcessTree>().unwrap();

        let snfa = StochasticNondeterministicFiniteAutomaton::from(tree);
        assert_eq!(snfa.sources, [a!(0), a!(1), a!(1), a!(2)]);
        assert_eq!(snfa.targets, [a!(1), a!(2), a!(2), a!(3)]);
        assert_eq!(
            snfa.probabilities,
            [f1!(), Fraction::from((1, 3)), Fraction::from((2, 3)), f1!()]
        );
        assert_eq!(snfa.termination_probabilities, [f0!(), f0!(), f0!(), f1!()]);
    }

    #[test]
    fn sptree_to_snfa_all_operators() {
        let fin = fs::read_to_string("testfiles/all_operators.sptree").unwrap();
        let tree = fin.parse::<StochasticProcessTree>().unwrap();

        let _snfa = StochasticNondeterministicFiniteAutomaton::from(tree);
    }

    #[test]
    fn sdfm_to_snfa() {
        let fin = fs::read_to_string("testfiles/aa-ab-ba.sdfm").unwrap();
        let sdfm = fin.parse::<StochasticDirectlyFollowsModel>().unwrap();

        let snfa = StochasticNondeterministicFiniteAutomaton::from(sdfm);

        assert_eq!(snfa.sources, [a!(0), a!(0), a!(1), a!(1), a!(2)]);
        assert_eq!(snfa.targets, [a!(1), a!(2), a!(1), a!(2), a!(1)]);
        assert_eq!(
            snfa.probabilities,
            [
                Fraction::from((2, 5)),
                Fraction::from((3, 5)),
                Fraction::from((1, 6)),
                Fraction::from((1, 6)),
                Fraction::from((3, 4))
            ]
        );
        assert_eq!(
            snfa.termination_probabilities,
            [
                Fraction::zero(),
                Fraction::from((2, 3)),
                Fraction::from((1, 4))
            ]
        );
    }
}
