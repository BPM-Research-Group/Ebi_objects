use ebi_bpmn::ebi_arithmetic::Fraction;

use crate::{AutomatonState, ebi_objects::labelled_petri_net::TransitionIndex};

pub trait StochasticAutomatonSemantics {
    /// Return the theoretical sum of weights of all ougoing transitions of this state.
    /// It is possible that the actual sum is lower; in this case; the stochastic language is incomplete.
    fn outgoing_transitions_weight_sum(&self, state: AutomatonState) -> Fraction;

    /// Returns the weight of the transition originating from the state.
    fn transition_2_weight(
        &self,
        state: AutomatonState,
        transition: TransitionIndex,
    ) -> Option<&Fraction>;
}
