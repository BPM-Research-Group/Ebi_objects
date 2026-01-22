use crate::{
    CompressedEventLog, CompressedEventLogTraceAttributes, CompressedEventLogXes, EventLog, EventLogCsv, EventLogPython, EventLogTraceAttributes, EventLogXes, FiniteStochasticLanguage, NumberOfTraces, StochasticDeterministicFiniteAutomaton, StochasticNondeterministicFiniteAutomaton
};
use ebi_arithmetic::{Fraction, One, Zero};
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
