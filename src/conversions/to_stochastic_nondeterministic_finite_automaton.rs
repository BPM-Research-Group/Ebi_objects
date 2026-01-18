use std::collections::{HashMap, hash_map::Entry};

use ebi_arithmetic::{Fraction, One, Zero};

use crate::{
    CompressedEventLog, CompressedEventLogTraceAttributes, CompressedEventLogXes, EventLog,
    EventLogCsv, EventLogTraceAttributes, EventLogXes, FiniteStochasticLanguage, NumberOfTraces,
    StochasticDeterministicFiniteAutomaton, StochasticNondeterministicFiniteAutomaton,
    ebi_objects::stochastic_nondeterministic_finite_automaton::{State, Transition},
};

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
                    state = result.take_or_add_transition(state, *activity, probability.clone());
                }

                match final_states.entry(state) {
                    Entry::Occupied(mut e) => *e.get_mut() += Fraction::one(),
                    Entry::Vacant(e) => {
                        e.insert(Fraction::one());
                    }
                }
            }

            //normalise
            result.scale_outgoing_probabilities();
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

        let mut states = terminating_probabilities
            .into_iter()
            .map(|termination_probability| State {
                termination_probability,
                transitions: vec![],
            })
            .collect::<Vec<_>>();

        for (source, (target, (activity, probability))) in sources.into_iter().zip(
            targets
                .into_iter()
                .zip(activities.into_iter().zip(probabilities.into_iter())),
        ) {
            states[source].transitions.push(Transition {
                target,
                label: Some(activity),
                probability,
            });
        }

        Self {
            activity_key,
            states,
            initial_state,
        }
    }
}
