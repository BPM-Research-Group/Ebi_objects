use crate::{
    AutomatonState,
    ebi_objects::{
        directly_follows_graph::DirectlyFollowsGraph,
        stochastic_directly_follows_model::StochasticDirectlyFollowsModel,
    },
};
use ebi_bpmn::ebi_arithmetic::{Fraction, Zero};

impl From<DirectlyFollowsGraph> for StochasticDirectlyFollowsModel {
    fn from(dfg: DirectlyFollowsGraph) -> Self {
        log::info!("Convert directly follows graph into stochastic directly follows model");

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
