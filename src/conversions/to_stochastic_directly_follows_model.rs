use crate::{
    AutomatonState, HasActivityKey,
    ebi_objects::{
        directly_follows_graph::DirectlyFollowsGraph,
        stochastic_directly_follows_model::StochasticDirectlyFollowsModel,
    },
};
use ebi_bpmn::ebi_arithmetic::{Fraction, Zero};

impl From<DirectlyFollowsGraph> for StochasticDirectlyFollowsModel {
    fn from(dfg: DirectlyFollowsGraph) -> Self {
        log::info!("Convert directly follows graph into stochastic directly follows model");

        let mut new_node_2_activity = vec![
            dfg.activity_key.get_activities()[0].clone();
            dfg.activity_key.get_number_of_activities()
        ];
        let mut start_node_weights = vec![Fraction::zero(); new_node_2_activity.len()];
        let mut end_node_weights = vec![Fraction::zero(); new_node_2_activity.len()];

        for activity in dfg.activity_key.get_activities() {
            let node = dfg.activity_key.get_id_from_activity(activity);

            new_node_2_activity[node] = activity.clone();
            start_node_weights[node] = dfg.start_activity_weight(*activity);
            end_node_weights[node] = dfg.end_activity_weight(*activity);
        }

        let DirectlyFollowsGraph {
            activity_key,
            empty_traces_weight,
            state_2_activity,
            sources,
            targets,
            weights,
            start_activities,
            end_activities,
            ..
        } = dfg;

        let sources = sources.into_iter().map(|x| x.0).collect();
        let targets = targets.into_iter().map(|x| x.0).collect();
        let start_node_weights = (0..state_2_activity.len())
            .map(|state| {
                start_activities
                    .get(AutomatonState::of(state))
                    .cloned()
                    .unwrap_or_else(|| Fraction::zero())
            })
            .collect();
        let end_node_weights = (0..state_2_activity.len())
            .map(|state| {
                end_activities
                    .get(AutomatonState::of(state))
                    .cloned()
                    .unwrap_or_else(|| Fraction::zero())
            })
            .collect();

        let result = Self {
            activity_key,
            node_2_activity: state_2_activity,
            empty_traces_weight,
            sources,
            targets,
            weights,
            start_node_weights,
            end_node_weights,
        };

        result
    }
}

#[cfg(test)]
mod tests {
    use std::fs;

    use crate::{DirectlyFollowsGraph, StochasticDirectlyFollowsModel};

    #[test]
    fn dfg_2_sdfm() {
        let fin = fs::read_to_string("testfiles/bpic12-a.xes.gz-dfg.dfg").unwrap();
        let dfg = fin.parse::<DirectlyFollowsGraph>().unwrap();

        let _dfm = StochasticDirectlyFollowsModel::from(dfg);
    }
}
