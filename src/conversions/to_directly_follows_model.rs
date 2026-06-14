use crate::{
    AutomatonState,
    ebi_objects::{
        directly_follows_graph::DirectlyFollowsGraph, directly_follows_model::DirectlyFollowsModel,
        stochastic_directly_follows_model::StochasticDirectlyFollowsModel,
    },
};
use ebi_bpmn::ebi_arithmetic::ebi_number::Signed;

impl From<DirectlyFollowsGraph> for DirectlyFollowsModel {
    fn from(value: DirectlyFollowsGraph) -> Self {
        log::info!("Convert directly follows graph into directly follows model");

        let start_nodes = (0..value.state_2_activity.len())
            .map(
                |state| match value.start_states.get(AutomatonState::of(state)) {
                    Some(weight) => weight.is_positive(),
                    None => false,
                },
            )
            .collect();

        let end_nodes = (0..value.state_2_activity.len())
            .map(
                |state| match value.start_states.get(AutomatonState::of(state)) {
                    Some(weight) => weight.is_positive(),
                    None => false,
                },
            )
            .collect();

        let DirectlyFollowsGraph {
            activity_key,
            empty_traces_weight,
            sources,
            targets,
            weights,
            state_2_activity,
            ..
        } = value;

        let (sources, targets) = sources
            .into_iter()
            .zip(targets.into_iter().zip(weights))
            .filter_map(|(s, (t, w))| if w.is_positive() { Some((s, t)) } else { None })
            .collect();

        let result = Self {
            activity_key: activity_key,
            empty_traces: empty_traces_weight.is_positive(),
            node_2_activity: state_2_activity,
            sources,
            targets,
            start_nodes,
            end_nodes,
        };

        result
    }
}

impl From<StochasticDirectlyFollowsModel> for DirectlyFollowsModel {
    fn from(value: StochasticDirectlyFollowsModel) -> Self {
        Self {
            activity_key: value.activity_key,
            node_2_activity: value.node_2_activity,
            empty_traces: value.empty_traces_weight.is_positive(),
            sources: value.sources,
            targets: value.targets,
            start_nodes: value
                .start_node_weights
                .into_iter()
                .map(|w| w.is_positive())
                .collect(),
            end_nodes: value
                .end_node_weights
                .into_iter()
                .map(|w| w.is_positive())
                .collect(),
        }
    }
}
