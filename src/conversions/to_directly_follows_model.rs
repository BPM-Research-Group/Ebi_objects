use crate::{
    HasActivityKey,
    ebi_objects::{
        directly_follows_graph::DirectlyFollowsGraph, directly_follows_model::DirectlyFollowsModel,
        stochastic_directly_follows_model::StochasticDirectlyFollowsModel,
    },
};
use ebi_bpmn::ebi_arithmetic::ebi_number::Signed;

impl From<DirectlyFollowsGraph> for DirectlyFollowsModel {
    fn from(value: DirectlyFollowsGraph) -> Self {
        log::info!("Convert directly follows graph into directly follows model");

        let node_2_activity = value
            .activity_key
            .get_activities()
            .iter()
            .cloned()
            .cloned()
            .collect::<Vec<_>>();

        let mut start_nodes = vec![false; node_2_activity.len()];
        value.start_activities.iter().for_each(|(node, weight)| {
            let activity = value.state_2_activity[node.0];
            if weight.is_positive() {
                start_nodes[value.activity_key.get_id_from_activity(activity)] = true;
            }
        });

        let mut end_nodes = vec![false; node_2_activity.len()];
        value.end_activities.iter().for_each(|(node, weight)| {
            let activity = value.state_2_activity[node.0];
            if weight.is_positive() {
                end_nodes[value.activity_key.get_id_from_activity(activity)] = true;
            }
        });

        let DirectlyFollowsGraph {
            activity_key,
            empty_traces_weight,
            sources,
            targets,
            weights,
            ..
        } = value;

        let mut result = Self {
            activity_key: activity_key,
            empty_traces: empty_traces_weight.is_positive(),
            node_2_activity: node_2_activity,
            sources: vec![],
            targets: vec![],
            start_nodes,
            end_nodes,
        };

        //edges
        for (source, (target, weight)) in sources.iter().zip(targets.iter().zip(weights.iter())) {
            let source = value.state_2_activity[source.0];
            let target = value.state_2_activity[target.0];
            if weight.is_positive() {
                let source_index = result.activity_key().get_id_from_activity(source);
                let target_index = result.activity_key().get_id_from_activity(target);
                result.add_edge(source_index, target_index)
            }
        }

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
