use crate::{
    HasActivityKey,
    ebi_objects::{
        directly_follows_graph::DirectlyFollowsGraph,
        stochastic_directly_follows_model::StochasticDirectlyFollowsModel,
    },
};
use ebi_bpmn::ebi_arithmetic::{Fraction, Signed, Zero};

impl From<DirectlyFollowsGraph> for StochasticDirectlyFollowsModel {
    fn from(value: DirectlyFollowsGraph) -> Self {
        log::info!("Convert directly follows graph into stochastic directly follows model");

        let mut new_node_2_activity = vec![
            value.activity_key.get_activities()[0].clone();
            value.activity_key.get_number_of_activities()
        ];
        let mut start_node_weights = vec![Fraction::zero(); new_node_2_activity.len()];
        let mut end_node_weights = vec![Fraction::zero(); new_node_2_activity.len()];

        for activity in value.activity_key.get_activities() {
            let node = value.activity_key.get_id_from_activity(activity);

            new_node_2_activity[node] = activity.clone();
            start_node_weights[node] = value.start_activity_weight(*activity);
            end_node_weights[node] = value.end_activity_weight(*activity);
        }

        let DirectlyFollowsGraph {
            activity_key,
            empty_traces_weight,
            state_2_activity,
            sources,
            targets,
            weights,
            ..
        } = value;

        let mut result = Self {
            activity_key: activity_key,
            node_2_activity: new_node_2_activity,
            empty_traces_weight,
            sources: vec![],
            targets: vec![],
            weights: vec![],
            start_node_weights,
            end_node_weights,
        };

        //edges
        for (source, (target, weight)) in sources
            .into_iter()
            .zip(targets.into_iter().zip(weights.into_iter()))
        {
            if weight.is_positive() {
                let source_index = result
                    .activity_key()
                    .get_id_from_activity(state_2_activity[source.0]);
                let target_index = result
                    .activity_key()
                    .get_id_from_activity(state_2_activity[target.0]);
                result.add_edge(source_index, target_index, weight)
            }
        }

        result
    }
}
