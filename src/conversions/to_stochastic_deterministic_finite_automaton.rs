use crate::{
    CompressedEventLog, CompressedEventLogXes, EventLogCsv, EventLogTraceAttributes, EventLogXes,
    HasActivityKey, NumberOfTraces,
    ebi_objects::{
        compressed_event_log_trace_attributes::CompressedEventLogTraceAttributes,
        event_log::EventLog, event_log_python::EventLogPython,
        finite_stochastic_language::FiniteStochasticLanguage,
        stochastic_deterministic_finite_automaton::StochasticDeterministicFiniteAutomaton,
    },
    traits::trace_iterators::{IntoRefTraceIterator, IntoTraceIterator},
};
use ebi_arithmetic::{Fraction, One, Zero};
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
    use crate::{FiniteStochasticLanguage, NumberOfTraces, StochasticDeterministicFiniteAutomaton};
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
}
