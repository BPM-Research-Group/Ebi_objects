use crate::{
    NumberOfTraces,
    activity_key::has_activity_key::HasActivityKey,
    ebi_objects::{
        deterministic_finite_automaton::DeterministicFiniteAutomaton, event_log::EventLog,
        finite_language::FiniteLanguage, finite_stochastic_language::FiniteStochasticLanguage,
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
            max_state: value.max_state,
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

impl From<EventLog> for DeterministicFiniteAutomaton {
    fn from(value: EventLog) -> Self {
        Into::<FiniteLanguage>::into(value).into()
    }
}
