use crate::{
    CompressedEventLog, CompressedEventLogTraceAttributes, CompressedEventLogXes,
    DirectlyFollowsGraph, EventLog, EventLogCsv, EventLogPython, EventLogTraceAttributes,
    EventLogXes, FiniteStochasticLanguage, HasActivityKey, NumberOfTraces,
    StochasticDeterministicFiniteAutomaton, StochasticNondeterministicFiniteAutomaton,
};
use ebi_arithmetic::{Fraction, One, Signed, Zero};
use std::collections::{HashMap, hash_map::Entry};

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
                    Entry::Occupied(mut e) => *e.get_mut() += Fraction::one(),
                    Entry::Vacant(e) => {
                        e.insert(Fraction::one());
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
                let log: FiniteStochasticLanguage = value.into();
                log.into()
            }
        }
    };
}

from_via_slang!(EventLog);
from_via_slang!(EventLogTraceAttributes);
from_via_slang!(EventLogXes);
from_via_slang!(EventLogCsv);
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
            terminating_probabilities,
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

        let final_state = result.add_state();

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
                        Some(*activity),
                        activity.id + 1,
                        weight / &sum_of_start_activities,
                    )
                    .unwrap(); //by construction, outgoing probability cannot become lower than 0
            }

            //emptytraces
            if value.empty_traces_weight.is_positive() {
                result
                    .add_transition(
                        0,
                        None,
                        final_state,
                        &value.empty_traces_weight / &sum_of_start_activities,
                    )
                    .unwrap(); //by construction, outgoing probabilities cannot become lower than 0
            }
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
                                Some(value.targets[i]),
                                value.targets[i].id + 1,
                                &value.weights[i] / &sum,
                            )
                            .unwrap(); //by construction, remaining outgoing probability cannot become negative
                    }
                    i += 1;
                }

                // termination
                if let Some(w) = value.end_activities.get(activity)
                    && w.is_positive()
                {
                    result
                        .add_transition(activity.id + 1, None, final_state, w / &sum)
                        .unwrap(); //by construction, remaining outgoing probability cannot become negative
                }
            }
        }

        result.activity_key = value.activity_key;

        result
    }
}

#[cfg(test)]
mod tests {
    use crate::{DirectlyFollowsGraph, HasActivityKey, StochasticNondeterministicFiniteAutomaton};
    use ebi_arithmetic::{Fraction, One, Zero, f0, f1};
    use std::fs;

    #[test]
    fn dfg_to_snfa() {
        let fin1 = fs::read_to_string("testfiles/aa-ab-ba.dfg").unwrap();
        let mut dfg: DirectlyFollowsGraph = fin1.parse::<DirectlyFollowsGraph>().unwrap();

        let a = dfg.activity_key_mut().process_activity("a");
        let b = dfg.activity_key_mut().process_activity("b");
        assert_eq!(dfg.start_activities[&a], Fraction::from((2, 5)));
        assert_eq!(dfg.start_activities[&b], Fraction::from((3, 5)));

        let snfa: StochasticNondeterministicFiniteAutomaton = dfg.into();
        assert_eq!(snfa.number_of_states(), 4);
        assert_eq!(snfa.number_of_transitions(), 7);
        assert_eq!(snfa.sources, [0, 0, 1, 1, 1, 2, 2]);
        assert_eq!(snfa.targets, [1, 2, 3, 1, 2, 3, 1]);
        assert_eq!(snfa.activities[2], None);
        assert_eq!(snfa.activities[5], None);
        assert_eq!(
            snfa.probabilities,
            [
                Fraction::from((2, 5)),
                Fraction::from((3, 5)),
                Fraction::from((2, 3)),
                Fraction::from((1, 6)),
                Fraction::from((1, 6)),
                Fraction::from((1, 4)),
                Fraction::from((3, 4))
            ]
        );
        assert_eq!(snfa.terminating_probabilities, [f0!(), f0!(), f0!(), f1!()]);
    }
}
