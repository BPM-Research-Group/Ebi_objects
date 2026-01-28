use crate::{
    CompressedEventLog, CompressedEventLogXes, DirectlyFollowsGraph, EventLogCsv,
    EventLogTraceAttributes, EventLogXes, HasActivityKey, NumberOfTraces,
    ebi_objects::{
        compressed_event_log_trace_attributes::CompressedEventLogTraceAttributes,
        event_log::EventLog, event_log_python::EventLogPython,
        finite_stochastic_language::FiniteStochasticLanguage,
        stochastic_deterministic_finite_automaton::StochasticDeterministicFiniteAutomaton,
    },
    traits::trace_iterators::{IntoRefTraceIterator, IntoTraceIterator},
};
use ebi_arithmetic::{Fraction, One, Signed, Zero};
use std::collections::{HashMap, hash_map::Entry};

impl From<FiniteStochasticLanguage> for StochasticDeterministicFiniteAutomaton {
    fn from(value: FiniteStochasticLanguage) -> Self {
        log::info!("convert finite stochastic language to SDFA");

        let mut result = StochasticDeterministicFiniteAutomaton::new();
        if value.number_of_traces().is_zero() {
            result.set_initial_state(None);
        } else {
            let mut final_states = HashMap::new();
            result.activity_key = value.activity_key.clone();

            //create automaton
            for (trace, probability) in &value.traces {
                let mut state = result.get_initial_state().unwrap();
                for activity in trace {
                    state = result.take_or_add_transition(state, *activity, probability.clone());
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

impl From<DirectlyFollowsGraph> for StochasticDeterministicFiniteAutomaton {
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
            for (activity, weight) in &value.start_activities {
                result
                    .add_transition(
                        initial_state,
                        *activity,
                        activity.id + 1,
                        weight / &sum_of_start_activities,
                    )
                    .unwrap(); //by construction, outgoing probability cannot become lower than 0
            }

            //emptytraces are implied
        }

        //edges
        {
            for activity in value.activity_key().get_activities() {
                //gather the outgoing sum
                let mut sum = Fraction::zero();
                {
                    let (_, mut i) =
                        value.binary_search(*activity, value.activity_key().get_activity_by_id(0));
                    while i < value.sources.len() && &value.sources[i] == activity {
                        if value.weights[i].is_positive() {
                            sum += &value.weights[i];
                        }
                        i += 1;
                    }

                    if let Some(w) = value.end_activities.get(activity)
                        && w.is_positive()
                    {
                        sum += w;
                    }
                }

                // add the edges
                let (_, mut i) =
                    value.binary_search(*activity, value.activity_key().get_activity_by_id(0));
                while i < value.sources.len() && &value.sources[i] == activity {
                    if value.weights[i].is_positive() {
                        result
                            .add_transition(
                                activity.id + 1,
                                value.targets[i],
                                value.targets[i].id + 1,
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

macro_rules! log {
    ($t: ident) => {
        impl From<$t> for StochasticDeterministicFiniteAutomaton {
            fn from(value: $t) -> Self {
                log::info!("convert event log to SDFA");

                let mut result = StochasticDeterministicFiniteAutomaton::new();
                result.activity_key = value.activity_key().clone();

                if value.number_of_traces().is_zero() {
                    result.set_initial_state(None);
                } else {
                    let mut final_states = HashMap::new();

                    //create automaton
                    for trace in value.iter_traces() {
                        let mut state = result.get_initial_state().unwrap();

                        for activity in trace {
                            state = result.take_or_add_transition(
                                state,
                                activity.clone(),
                                Fraction::one(),
                            );
                        }

                        match final_states.entry(state) {
                            std::collections::hash_map::Entry::Occupied(mut e) => {
                                *e.get_mut() += Fraction::one()
                            }
                            std::collections::hash_map::Entry::Vacant(e) => {
                                e.insert(Fraction::one());
                            }
                        }
                    }

                    //count
                    let mut sums = final_states;
                    for (source, _, _, probability) in &result {
                        match sums.entry(*source) {
                            std::collections::hash_map::Entry::Occupied(mut e) => {
                                *e.get_mut() += probability
                            }
                            std::collections::hash_map::Entry::Vacant(e) => {
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
    };
}

macro_rules! from_via_log {
    ($t:ident) => {
        impl From<$t> for StochasticDeterministicFiniteAutomaton {
            fn from(value: $t) -> Self {
                let log: EventLog = value.into();
                log.into()
            }
        }
    };
}

log!(EventLog);
log!(EventLogTraceAttributes);
log!(EventLogXes);
log!(EventLogCsv);
from_via_log!(EventLogPython);
from_via_log!(CompressedEventLogXes);
from_via_log!(CompressedEventLog);
from_via_log!(CompressedEventLogTraceAttributes);

#[cfg(test)]
mod tests {
    use ebi_arithmetic::{Fraction, Zero, f0};

    use crate::{
        DirectlyFollowsGraph, FiniteStochasticLanguage, HasActivityKey, NumberOfTraces,
        StochasticDeterministicFiniteAutomaton,
    };
    use std::fs;

    #[test]
    fn slang_minprob_zero_through_sdfa() {
        let fin = fs::read_to_string("testfiles/aa-ab-ba.slang").unwrap();
        let slang = fin.parse::<FiniteStochasticLanguage>().unwrap();
        assert_eq!(slang.number_of_traces(), 3);
        let sdfa: StochasticDeterministicFiniteAutomaton = slang.into();
        assert_eq!(sdfa.number_of_states(), 6);
        assert_eq!(sdfa.number_of_transitions(), 5);
    }

    #[test]
    fn dfg_to_sdfa() {
        let fin1 = fs::read_to_string("testfiles/aa-ab-ba.dfg").unwrap();
        let mut dfg: DirectlyFollowsGraph = fin1.parse::<DirectlyFollowsGraph>().unwrap();

        let a = dfg.activity_key_mut().process_activity("a");
        let b = dfg.activity_key_mut().process_activity("b");
        assert_eq!(dfg.start_activities[&a], Fraction::from((2, 5)));
        assert_eq!(dfg.start_activities[&b], Fraction::from((3, 5)));

        let snfa: StochasticDeterministicFiniteAutomaton = dfg.into();
        assert_eq!(snfa.number_of_states(), 3);
        assert_eq!(snfa.number_of_transitions(), 5);
        assert_eq!(snfa.sources, [0, 0, 1, 1, 2]);
        assert_eq!(snfa.targets, [1, 2, 1, 2, 1]);
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
            snfa.terminating_probabilities,
            [f0!(), Fraction::from((2, 3)), Fraction::from((1, 4))]
        );
    }

    #[test]
    fn log_to_sdfa() {
        let fin1 = fs::read_to_string("testfiles/acb-abc-ad-aded-adeded-adededed.slang").unwrap();
        let slang = fin1.parse::<FiniteStochasticLanguage>().unwrap();

        let sdfa = StochasticDeterministicFiniteAutomaton::from(slang);

        assert!(
            sdfa.terminating_probabilities
                .contains(&Fraction::from((8, 15)))
        );
        assert!(
            sdfa.terminating_probabilities
                .contains(&Fraction::from((2, 3)))
        );
        assert!(
            sdfa.terminating_probabilities
                .contains(&Fraction::from((4, 7)))
        );
    }
}
