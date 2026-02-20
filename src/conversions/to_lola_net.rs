use crate::{
    BusinessProcessModelAndNotation, StochasticNondeterministicFiniteAutomaton,
    ebi_objects::{
        deterministic_finite_automaton::DeterministicFiniteAutomaton,
        directly_follows_graph::DirectlyFollowsGraph, directly_follows_model::DirectlyFollowsModel,
        labelled_petri_net::LabelledPetriNet, lola_net::LolaNet, process_tree::ProcessTree,
        stochastic_deterministic_finite_automaton::StochasticDeterministicFiniteAutomaton,
        stochastic_directly_follows_model::StochasticDirectlyFollowsModel,
        stochastic_labelled_petri_net::StochasticLabelledPetriNet,
        stochastic_process_tree::StochasticProcessTree,
    },
};
use anyhow::Error;

impl From<LabelledPetriNet> for LolaNet {
    fn from(value: LabelledPetriNet) -> Self {
        log::info!("Convert LPN into Lola net.");
        Self(value)
    }
}

macro_rules! via_lpn {
    ($t:ident) => {
        impl From<$t> for LolaNet {
            fn from(value: $t) -> Self {
                let lpn: LabelledPetriNet = value.into();
                lpn.into()
            }
        }
    };
}

via_lpn!(StochasticLabelledPetriNet);
via_lpn!(DeterministicFiniteAutomaton);
via_lpn!(DirectlyFollowsModel);
via_lpn!(DirectlyFollowsGraph);
via_lpn!(StochasticDirectlyFollowsModel);
via_lpn!(ProcessTree);
via_lpn!(StochasticProcessTree);
via_lpn!(StochasticDeterministicFiniteAutomaton);
via_lpn!(StochasticNondeterministicFiniteAutomaton);

impl TryFrom<BusinessProcessModelAndNotation> for LolaNet {
    type Error = Error;

    fn try_from(value: BusinessProcessModelAndNotation) -> Result<Self, Self::Error> {
        let lpn: LabelledPetriNet = value.try_into()?;
        Ok(Self(lpn))
    }
}
