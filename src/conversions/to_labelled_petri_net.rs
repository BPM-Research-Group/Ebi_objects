use crate::{
    ActivityKeyTranslator, HasActivityKey, PetriNetMarkupLanguage,
    StochasticNondeterministicFiniteAutomaton,
    ebi_objects::{
        deterministic_finite_automaton::DeterministicFiniteAutomaton,
        directly_follows_graph::DirectlyFollowsGraph,
        directly_follows_model::DirectlyFollowsModel,
        labelled_petri_net::LabelledPetriNet,
        lola_net::LolaNet,
        partially_ordered_workflow_language::{Model, PartiallyOrderedWorkflowLanguage},
        process_tree::{Node, Operator, ProcessTree},
        process_tree_markup_language::ProcessTreeMarkupLanguage,
        stochastic_deterministic_finite_automaton::StochasticDeterministicFiniteAutomaton,
        stochastic_directly_follows_model::StochasticDirectlyFollowsModel,
        stochastic_labelled_petri_net::StochasticLabelledPetriNet,
        stochastic_process_tree::StochasticProcessTree,
    },
    marking::Marking,
};
use ebi_activity_key::Activity;
use ebi_bpmn::ebi_arithmetic::anyhow::{Error, Result, anyhow};
use std::collections::HashMap;

macro_rules! tree {
    ($t:ident) => {
        impl From<$t> for LabelledPetriNet {
            fn from(value: $t) -> Self {
                log::info!("convert (stochastic) process tree to LPN");

                if value.tree.is_empty() {
                    return Self::new_empty_language();
                }

                let mut result = LabelledPetriNet::new();
                let translator =
                    ActivityKeyTranslator::new(value.activity_key(), result.activity_key_mut());
                let source = result.add_place();
                let sink = result.add_place();
                result
                    .get_initial_marking_mut()
                    .as_mut()
                    .unwrap()
                    .increase(source, 1)
                    .unwrap();

                value
                    .node_to_lpn(0, &mut result, &translator, source, sink)
                    .unwrap();

                result
            }
        }

        impl $t {
            pub fn node_to_lpn(
                &self,
                node: usize,
                net: &mut LabelledPetriNet,
                translator: &ActivityKeyTranslator,
                source: usize,
                sink: usize,
            ) -> Result<usize> {
                match self.tree[node] {
                    Node::Tau => {
                        let transition = net.add_transition(None);
                        net.add_place_transition_arc(source, transition, 1)?;
                        net.add_transition_place_arc(transition, sink, 1)?;
                        Ok(node + 1)
                    }
                    Node::Activity(activity) => {
                        let transition =
                            net.add_transition(Some(translator.translate_activity(&activity)));
                        net.add_place_transition_arc(source, transition, 1)?;
                        net.add_transition_place_arc(transition, sink, 1)?;
                        Ok(node + 1)
                    }
                    Node::Operator(Operator::Concurrent, number_of_children) => {
                        let split = net.add_transition(None);
                        net.add_place_transition_arc(source, split, 1)?;
                        let join = net.add_transition(None);
                        net.add_transition_place_arc(join, sink, 1)?;

                        let mut child = node + 1;
                        for _ in 0..number_of_children {
                            let child_source = net.add_place();
                            net.add_transition_place_arc(split, child_source, 1)?;
                            let child_sink = net.add_place();
                            net.add_place_transition_arc(child_sink, join, 1)?;
                            child =
                                self.node_to_lpn(child, net, translator, child_source, child_sink)?;
                        }
                        Ok(child)
                    }
                    Node::Operator(Operator::Interleaved, number_of_children) => {
                        let split = net.add_transition(None);
                        net.add_place_transition_arc(source, split, 1)?;
                        let join = net.add_transition(None);
                        net.add_transition_place_arc(join, sink, 1)?;
                        let milestone = net.add_place();
                        net.add_transition_place_arc(split, milestone, 1)?;
                        net.add_place_transition_arc(milestone, join, 1)?;

                        let mut child = node + 1;
                        for _ in 0..number_of_children {
                            let child_source = net.add_place();
                            net.add_transition_place_arc(split, child_source, 1)?;

                            let child_start = net.add_transition(None);
                            net.add_place_transition_arc(child_source, child_start, 1)?;
                            net.add_place_transition_arc(milestone, child_start, 1)?;

                            let child_source_2 = net.add_place();
                            net.add_transition_place_arc(child_start, child_source_2, 1)?;

                            let child_sink = net.add_place();
                            net.add_place_transition_arc(child_sink, join, 1)?;

                            let child_stop = net.add_transition(None);
                            net.add_transition_place_arc(child_stop, child_sink, 1)?;
                            net.add_transition_place_arc(child_stop, milestone, 1)?;

                            let child_sink_2 = net.add_place();
                            net.add_place_transition_arc(child_sink_2, child_stop, 1)?;

                            child = self.node_to_lpn(
                                child,
                                net,
                                translator,
                                child_source_2,
                                child_sink_2,
                            )?;
                        }
                        Ok(child)
                    }
                    Node::Operator(Operator::Loop, number_of_children) => {
                        let start = net.add_transition(None);
                        net.add_place_transition_arc(source, start, 1)?;

                        let join = net.add_place();
                        net.add_transition_place_arc(start, join, 1)?;

                        let split = net.add_place();
                        let stop = net.add_transition(None);
                        net.add_place_transition_arc(split, stop, 1)?;
                        net.add_transition_place_arc(stop, sink, 1)?;

                        let mut child = node + 1;
                        child = self.node_to_lpn(child, net, translator, join, split)?;

                        if number_of_children > 1 {
                            for _ in 1..number_of_children {
                                child = self.node_to_lpn(child, net, translator, split, join)?;
                            }
                        } else {
                            let redo = net.add_transition(None);
                            net.add_place_transition_arc(split, redo, 1)?;
                            net.add_transition_place_arc(redo, join, 1)?;
                        }

                        Ok(child)
                    }
                    Node::Operator(Operator::Or, number_of_children) => {
                        let start = net.add_transition(None);
                        net.add_place_transition_arc(source, start, 1)?;

                        let not_done_first = net.add_place();
                        net.add_transition_place_arc(start, not_done_first, 1)?;

                        let done_first = net.add_place();
                        let end = net.add_transition(None);
                        net.add_place_transition_arc(done_first, end, 1)?;
                        net.add_transition_place_arc(end, sink, 1)?;

                        let mut child = node + 1;
                        for _ in 0..number_of_children {
                            let child_source = net.add_place();
                            net.add_transition_place_arc(start, child_source, 1)?;
                            let child_sink = net.add_place();
                            net.add_place_transition_arc(child_sink, end, 1)?;
                            let do_child = net.add_place();

                            //skip
                            let skip_child = net.add_transition(None);
                            net.add_place_transition_arc(child_source, skip_child, 1)?;
                            net.add_transition_place_arc(skip_child, child_sink, 1)?;
                            net.add_transition_place_arc(skip_child, done_first, 1)?;
                            net.add_place_transition_arc(done_first, skip_child, 1)?;

                            //first do
                            let first_do_child = net.add_transition(None);
                            net.add_place_transition_arc(child_source, first_do_child, 1)?;
                            net.add_place_transition_arc(not_done_first, first_do_child, 1)?;
                            net.add_transition_place_arc(first_do_child, done_first, 1)?;
                            net.add_transition_place_arc(first_do_child, do_child, 1)?;

                            //later do
                            let later_do_child = net.add_transition(None);
                            net.add_place_transition_arc(child_source, later_do_child, 1)?;
                            net.add_transition_place_arc(later_do_child, do_child, 1)?;
                            net.add_transition_place_arc(later_do_child, done_first, 1)?;
                            net.add_place_transition_arc(done_first, later_do_child, 1)?;

                            child =
                                self.node_to_lpn(child, net, translator, do_child, child_sink)?;
                        }

                        Ok(child)
                    }
                    Node::Operator(Operator::Sequence, number_of_children) => {
                        let intermediate_nodes = (0..(number_of_children - 1))
                            .map(|_| net.add_place())
                            .collect::<Vec<_>>();

                        let mut child = node + 1;
                        for i in 0..number_of_children {
                            let child_entry = if i == 0 {
                                source
                            } else {
                                intermediate_nodes[i - 1]
                            };
                            let child_exit = if i == number_of_children - 1 {
                                sink
                            } else {
                                intermediate_nodes[i]
                            };

                            child = $t::node_to_lpn(
                                &self,
                                child,
                                net,
                                translator,
                                child_entry,
                                child_exit,
                            )?;
                        }
                        Ok(child)
                    }
                    Node::Operator(Operator::Xor, number_of_children) => {
                        let mut child = node + 1;
                        for _ in 0..number_of_children {
                            child = $t::node_to_lpn(&self, child, net, translator, source, sink)?;
                        }
                        Ok(child)
                    }
                }
            }
        }
    };
}

tree!(ProcessTree);
tree!(StochasticProcessTree);

impl From<PartiallyOrderedWorkflowLanguage> for LabelledPetriNet {
    fn from(value: PartiallyOrderedWorkflowLanguage) -> Self {
        log::info!("convert partially ordered workflow language to LPN");
        let mut result = Self::new_with_activity_key(value.activity_key.clone());
        let source = result.add_place();
        let sink = result.add_place();

        result
            .get_initial_marking_mut()
            .as_mut()
            .unwrap()
            .increase(source, 1)
            .unwrap();

        powl_model_multiplicity(&value.root_model, &mut result, source, sink);
        result
    }
}

fn powl_model_multiplicity(
    model: &Model,
    result: &mut LabelledPetriNet,
    source: usize,
    sink: usize,
) {
    //first, handle skippable and repeatable
    match model {
        Model::Activity {
            skippable,
            repeatable,
            ..
        }
        | Model::PartialOrder {
            skippable,
            repeatable,
            ..
        }
        | Model::ChoiceGraph {
            skippable,
            repeatable,
            ..
        } => {
            let (new_source, new_sink) = match (skippable, repeatable) {
                (true, true) => {
                    //zero or more times
                    let middle_place = result.add_place();

                    let entry = result.add_transition(None);
                    result.add_place_transition_arc(source, entry, 1).unwrap();
                    result
                        .add_transition_place_arc(entry, middle_place, 1)
                        .unwrap();

                    let exit = result.add_transition(None);
                    result
                        .add_place_transition_arc(middle_place, exit, 1)
                        .unwrap();
                    result.add_transition_place_arc(exit, sink, 1).unwrap();

                    (middle_place, middle_place)
                }
                (true, false) => {
                    //zero or one time
                    let silent = result.add_transition(None);
                    result.add_place_transition_arc(source, silent, 1).unwrap();
                    result.add_transition_place_arc(silent, sink, 1).unwrap();

                    (source, sink)
                }
                (false, true) => {
                    //one or more times
                    let new_source = result.add_place();
                    let new_sink = result.add_place();

                    let entry = result.add_transition(None);
                    result.add_place_transition_arc(source, entry, 1).unwrap();
                    result
                        .add_transition_place_arc(entry, new_source, 1)
                        .unwrap();

                    let exit = result.add_transition(None);
                    result.add_place_transition_arc(new_sink, exit, 1).unwrap();
                    result.add_transition_place_arc(exit, sink, 1).unwrap();

                    let redo = result.add_transition(None);
                    result.add_place_transition_arc(new_sink, redo, 1).unwrap();
                    result
                        .add_transition_place_arc(redo, new_source, 1)
                        .unwrap();

                    (new_source, new_sink)
                }
                (false, false) => {
                    //once
                    (source, sink)
                }
            };

            powl_model_2_lpn(model, result, new_source, new_sink);
        }
    }

    fn powl_model_2_lpn(model: &Model, result: &mut LabelledPetriNet, source: usize, sink: usize) {
        match model {
            Model::Activity { activity, .. } => powl_activity_2_lpn(activity, result, source, sink),
            Model::PartialOrder { nodes, edges, .. } => {
                powl_partial_order_2_lpn(nodes, edges, result, source, sink)
            }
            Model::ChoiceGraph {
                nodes,
                edges,
                start_nodes,
                end_nodes,
                ..
            } => {
                powl_choice_graph_2_lpn(nodes, edges, start_nodes, end_nodes, result, source, sink)
            }
        }
    }

    fn powl_activity_2_lpn(
        activity: &Option<Activity>,
        result: &mut LabelledPetriNet,
        source: usize,
        sink: usize,
    ) {
        let transition = result.add_transition(*activity);
        result
            .add_place_transition_arc(source, transition, 1)
            .unwrap();
        result
            .add_transition_place_arc(transition, sink, 1)
            .unwrap();
    }
}

fn powl_partial_order_2_lpn(
    nodes: &[Model],
    edges: &[(usize, usize)],
    result: &mut LabelledPetriNet,
    source: usize,
    sink: usize,
) {
    //children
    let index_2_handles = nodes
        .iter()
        .map(|node| {
            let child_source = result.add_place();
            let child_sink = result.add_place();

            powl_model_multiplicity(node, result, child_source, child_sink);

            let child_entry = result.add_transition(None);
            result
                .add_transition_place_arc(child_entry, child_source, 1)
                .unwrap();

            let child_exit = result.add_transition(None);
            result
                .add_place_transition_arc(child_sink, child_exit, 1)
                .unwrap();

            (child_entry, child_exit)
        })
        .collect::<Vec<_>>();

    let mut is_start = vec![true; nodes.len()];
    let mut is_end = vec![true; nodes.len()];

    //edges
    for (source_index, target_index) in edges {
        let entry = index_2_handles[*source_index].1;
        let exit = index_2_handles[*target_index].0;

        let place = result.add_place();

        result.add_transition_place_arc(entry, place, 1).unwrap();
        result.add_place_transition_arc(place, exit, 1).unwrap();

        is_start[*target_index] = false;
        is_end[*source_index] = false;
    }

    //start
    let entry = result.add_transition(None);
    result.add_place_transition_arc(source, entry, 1).unwrap();
    for (target_index, x) in is_start.into_iter().enumerate() {
        if x {
            let target = index_2_handles[target_index].0;
            let place = result.add_place();
            result.add_transition_place_arc(entry, place, 1).unwrap();
            result.add_place_transition_arc(place, target, 1).unwrap();
        }
    }

    let exit = result.add_transition(None);
    result.add_transition_place_arc(exit, sink, 1).unwrap();
    for (source_index, x) in is_end.into_iter().enumerate() {
        if x {
            let source = index_2_handles[source_index].1;
            let place = result.add_place();
            result.add_transition_place_arc(source, place, 1).unwrap();
            result.add_place_transition_arc(place, exit, 1).unwrap();
        }
    }
}

fn powl_choice_graph_2_lpn(
    nodes: &[Model],
    edges: &[(usize, usize)],
    start_nodes: &[usize],
    end_nodes: &[usize],
    result: &mut LabelledPetriNet,
    source: usize,
    sink: usize,
) {
    //children
    let index_2_handles = nodes
        .iter()
        .map(|node| {
            let child_source = result.add_place();
            let child_sink = result.add_place();

            powl_model_multiplicity(node, result, child_source, child_sink);

            (child_source, child_sink)
        })
        .collect::<Vec<_>>();

    //edges
    for (source_index, target_index) in edges {
        let source = index_2_handles[*source_index].1;
        let target = index_2_handles[*target_index].0;
        let transition = result.add_transition(None);
        result
            .add_place_transition_arc(source, transition, 1)
            .unwrap();
        result
            .add_transition_place_arc(transition, target, 1)
            .unwrap();
    }

    //start edges
    for target_index in start_nodes {
        let transition = result.add_transition(None);
        let target = index_2_handles[*target_index].0;
        result
            .add_place_transition_arc(source, transition, 1)
            .unwrap();
        result
            .add_transition_place_arc(transition, target, 1)
            .unwrap();
    }

    //end edges
    for source_index in end_nodes {
        let transition = result.add_transition(None);
        let source = index_2_handles[*source_index].1;
        result
            .add_place_transition_arc(source, transition, 1)
            .unwrap();
        result
            .add_transition_place_arc(transition, sink, 1)
            .unwrap();
    }
}

impl From<ProcessTreeMarkupLanguage> for LabelledPetriNet {
    fn from(value: ProcessTreeMarkupLanguage) -> Self {
        value.tree.into()
    }
}

impl From<LolaNet> for LabelledPetriNet {
    fn from(value: LolaNet) -> Self {
        value.0
    }
}

impl From<PetriNetMarkupLanguage> for LabelledPetriNet {
    fn from(value: PetriNetMarkupLanguage) -> Self {
        value.0
    }
}

impl From<DirectlyFollowsModel> for LabelledPetriNet {
    fn from(value: DirectlyFollowsModel) -> LabelledPetriNet {
        log::info!("convert DFM to LPN");

        if value.node_2_activity.is_empty() && !value.empty_traces {
            //SDFA has an empty language, return a livelocked SLPN
            return Self::new_empty_language();
        }

        let mut result = LabelledPetriNet::new();
        let translator =
            ActivityKeyTranslator::new(value.activity_key(), result.activity_key_mut());
        let source = result.add_place();
        let sink = result.add_place();
        result
            .get_initial_marking_mut()
            .as_mut()
            .unwrap()
            .increase(source, 1)
            .unwrap();

        /*
         * empty traces
         */
        if value.empty_traces {
            let transition = result.add_transition(None);

            result
                .add_place_transition_arc(source, transition, 1)
                .unwrap();
            result
                .add_transition_place_arc(transition, sink, 1)
                .unwrap();
        }

        /*
         * Nodes (states): after doing a node you end up in the corresponding place.
         */
        let mut node2place = vec![];
        for _ in 0..value.node_2_activity.len() {
            let place = result.add_place();
            node2place.push(place);
        }

        /*
         * Transitions
         */
        for (source_node, target_node) in value.sources.iter().zip(value.targets.iter()) {
            let from_place = node2place[*source_node];
            let to_place = node2place[*target_node];
            let activity = translator.translate_activity(&value.node_2_activity[*target_node]);
            let transition = result.add_transition(Some(activity));

            result
                .add_place_transition_arc(from_place, transition, 1)
                .unwrap();
            result
                .add_transition_place_arc(transition, to_place, 1)
                .unwrap();
        }

        /*
         * Starts
         */
        for node in 0..value.node_2_activity.len() {
            if value.start_nodes[node] {
                let activity = translator.translate_activity(&value.node_2_activity[node]);
                let transition = result.add_transition(Some(activity));
                result
                    .add_place_transition_arc(source, transition, 1)
                    .unwrap();
                let target_place = node2place[node];
                result
                    .add_transition_place_arc(transition, target_place, 1)
                    .unwrap();
            }
        }

        /*
         * Ends
         */
        for node in 0..value.node_2_activity.len() {
            if value.end_nodes[node] {
                let transition = result.add_transition(None);
                let source_place = node2place[node];
                result
                    .add_place_transition_arc(source_place, transition, 1)
                    .unwrap();
                result
                    .add_transition_place_arc(transition, sink, 1)
                    .unwrap();
            }
        }

        result
    }
}

impl From<StochasticLabelledPetriNet> for LabelledPetriNet {
    fn from(value: StochasticLabelledPetriNet) -> Self {
        log::info!("convert SLPN to LPN");
        Self {
            activity_key: value.activity_key,
            initial_marking: value.initial_marking,
            labels: value.labels,
            place2output_transitions: value.place2output_transitions,
            transition2input_places: value.transition2input_places,
            transition2output_places: value.transition2output_places,
            transition2input_places_cardinality: value.transition2input_places_cardinality,
            transition2output_places_cardinality: value.transition2output_places_cardinality,
        }
    }
}

impl From<DeterministicFiniteAutomaton> for LabelledPetriNet {
    fn from(value: DeterministicFiniteAutomaton) -> Self {
        log::info!("convert DFA to LPN");

        let source = if let Some(s) = value.initial_state {
            s
        } else {
            return Self::new_empty_language();
        };

        let mut result = LabelledPetriNet::new();
        let translator =
            ActivityKeyTranslator::new(value.activity_key(), result.activity_key_mut());

        //add places
        let mut state2place = vec![];
        for state in 0..value.final_states.len() {
            let lpn_place = result.add_place();
            state2place.push(lpn_place);

            //add termination
            if value.final_states[state] {
                let lpn_transition = result.add_transition(None);
                result
                    .add_place_transition_arc(lpn_place, lpn_transition, 1)
                    .unwrap();
            }
        }

        //initial marking
        result
            .get_initial_marking_mut()
            .as_mut()
            .unwrap()
            .increase(source.0, 1)
            .unwrap();

        //add edges
        for (source, (target, activity)) in value
            .sources
            .iter()
            .zip(value.targets.iter().zip(value.activities.iter()))
        {
            //add transition
            let lpn_activity = translator.translate_activity(activity);
            let lpn_transition = result.add_transition(Some(lpn_activity));
            let source_place = state2place[*source];
            let target_place = state2place[*target];
            result
                .add_place_transition_arc(source_place, lpn_transition, 1)
                .unwrap();
            result
                .add_transition_place_arc(lpn_transition, target_place, 1)
                .unwrap();
        }

        result
    }
}

impl TryFrom<process_mining::PetriNet> for LabelledPetriNet {
    type Error = Error;

    fn try_from(pnml: process_mining::PetriNet) -> Result<Self, Self::Error> {
        log::info!("convert PNML to LPN");

        let mut result = LabelledPetriNet::new();

        //create map of places
        let mut place2index = HashMap::new();
        for (place_id, _) in pnml.places {
            let place = result.add_place();
            place2index.insert(place_id, place);
        }

        //transitions
        let mut transition2index = HashMap::new();
        for (transition_id, transition) in &pnml.transitions {
            let label = match &transition.label {
                Some(activity) => Some(result.activity_key_mut().process_activity(activity)),
                None => None,
            };
            let transition = result.add_transition(label);

            transition2index.insert(transition_id, transition);
        }

        //arcs
        for arc in pnml.arcs.iter() {
            match arc.from_to {
                process_mining::core::process_models::petri_net::ArcType::PlaceTransition(
                    place_id,
                    transition_id,
                ) => {
                    let new_place = place2index
                        .get(&place_id)
                        .ok_or(anyhow!("Undeclared place referenced."))?;
                    let new_transition = transition2index
                        .get(&transition_id)
                        .ok_or(anyhow!("undeclared transition referenced"))?;
                    result.add_place_transition_arc(
                        *new_place,
                        *new_transition,
                        arc.weight.into(),
                    )?;
                }
                process_mining::core::process_models::petri_net::ArcType::TransitionPlace(
                    transition_id,
                    place_id,
                ) => {
                    let new_place = place2index
                        .get(&place_id)
                        .ok_or(anyhow!("Undeclared place referenced."))?;
                    let new_transition = transition2index
                        .get(&transition_id)
                        .ok_or(anyhow!("undeclared transition referenced"))?;
                    result.add_transition_place_arc(
                        *new_transition,
                        *new_place,
                        arc.weight.into(),
                    )?;
                }
            };
        }

        //initial marking
        // for (place_id, cardinality) in pnml.net.initial_marking.as_ref().ok_or(anyhow!("The given net has no initial marking. Ebi requires an initial marking for its Petri nets."))?.iter() {
        if let Some(marking) = pnml.initial_marking.as_ref() {
            for (place_id, cardinality) in marking {
                let new_place = place2index
                    .get(&place_id.get_uuid())
                    .ok_or(anyhow!("Undeclared place found in the initial marking."))?;
                result
                    .get_initial_marking_mut()
                    .as_mut()
                    .unwrap()
                    .increase(*new_place, *cardinality)?;
            }
        }

        //final markings
        if let Some(final_markings) = &pnml.final_markings {
            //The nets used by Ebi do not have final markings, as each of their deadlocks is taken as a final marking.
            //The best we can do here is to verify that no non-deadlocks have been declared as final markings.
            for final_marking in final_markings.iter() {
                //transform to an Ebi-final marking
                let mut new_final_marking = Marking::new(result.get_number_of_places());
                for (place_id, cardinality) in final_marking.iter() {
                    let new_place = place2index
                        .get(&place_id.get_uuid())
                        .ok_or(anyhow!("Undeclared place found."))?;
                    new_final_marking.increase(*new_place, *cardinality)?;
                }

                //verify that this is a deadlock marking
                for transition in 0..result.transition2input_places.len() {
                    let mut marking = new_final_marking.clone();
                    let mut enabled = true;
                    for (in_place_pos, in_place) in result.transition2input_places[transition]
                        .iter()
                        .enumerate()
                    {
                        if marking.place2token[*in_place]
                            < result.transition2input_places_cardinality[transition][in_place_pos]
                        {
                            //transition not enabled
                            enabled = false;
                            break;
                        } else {
                            //transition may be enabled
                            marking.place2token[*in_place] -=
                                result.transition2input_places_cardinality[transition][in_place_pos]
                        }
                    }

                    if enabled {
                        return Err(anyhow!(
                            "This PNML file has a final marking that is not a deadlock. In Ebi, each final marking must be a deadlock. This final marking is {:?}",
                            final_marking
                        ));
                    }
                }
            }
        }

        Ok(result)
    }
}

impl From<StochasticDirectlyFollowsModel> for LabelledPetriNet {
    fn from(value: StochasticDirectlyFollowsModel) -> Self {
        let dfm = DirectlyFollowsModel::from(value);
        dfm.into()
    }
}

impl From<StochasticDeterministicFiniteAutomaton> for LabelledPetriNet {
    fn from(value: StochasticDeterministicFiniteAutomaton) -> Self {
        let dfa: DeterministicFiniteAutomaton = value.into();
        dfa.into()
    }
}

impl From<StochasticNondeterministicFiniteAutomaton> for LabelledPetriNet {
    fn from(value: StochasticNondeterministicFiniteAutomaton) -> Self {
        let slpn = StochasticLabelledPetriNet::from(value);
        slpn.into()
    }
}

impl From<DirectlyFollowsGraph> for LabelledPetriNet {
    fn from(value: DirectlyFollowsGraph) -> Self {
        <DirectlyFollowsGraph as Into<DirectlyFollowsModel>>::into(value).into()
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        StochasticDeterministicFiniteAutomaton,
        ebi_objects::{
            deterministic_finite_automaton::DeterministicFiniteAutomaton,
            labelled_petri_net::LabelledPetriNet,
            partially_ordered_workflow_language::PartiallyOrderedWorkflowLanguage,
        },
    };
    use std::fs;

    #[test]
    fn dfa_to_lpn() {
        let fin = fs::read_to_string("testfiles/a-loop.dfa").unwrap();
        let dfa = fin.parse::<DeterministicFiniteAutomaton>().unwrap();
        let lpn: LabelledPetriNet = dfa.into();

        let fin2 = fs::read_to_string("testfiles/a-loop.lpn").unwrap();
        let lpn2 = fin2.parse::<LabelledPetriNet>().unwrap();

        assert_eq!(lpn.to_string(), lpn2.to_string());
    }

    #[test]
    fn sdfa_dfa_lpn() {
        let fin = fs::read_to_string("testfiles/a-livelock-zeroweight.sdfa").unwrap();
        let sdfa = fin
            .parse::<StochasticDeterministicFiniteAutomaton>()
            .unwrap();
        let _lpn: LabelledPetriNet = sdfa.into();
    }

    #[test]
    fn powl_2_lpn_request_handling() {
        let fin = fs::read_to_string("testfiles/request_handling.powl").unwrap();
        let powl = fin.parse::<PartiallyOrderedWorkflowLanguage>().unwrap();

        // fs::write("powl.svg", powl.to_svg().unwrap().to_string()).unwrap();

        let _lpn = LabelledPetriNet::from(powl);

        // fs::write("lpn.svg", lpn.to_svg().unwrap().to_string()).unwrap();
    }

    #[test]
    fn powl_2_lpn_a() {
        let fin = fs::read_to_string("testfiles/a.powl").unwrap();
        let powl = fin.parse::<PartiallyOrderedWorkflowLanguage>().unwrap();

        // fs::write("powl.svg", powl.to_svg().unwrap().to_string()).unwrap();

        let _lpn = LabelledPetriNet::from(powl);

        // fs::write("lpn.svg", lpn.to_svg().unwrap().to_string()).unwrap();
    }

    #[test]
    fn powl_2_lpn_and_a_b() {
        let fin = fs::read_to_string("testfiles/and_a_b.powl").unwrap();
        let powl = fin.parse::<PartiallyOrderedWorkflowLanguage>().unwrap();

        // fs::write("powl.svg", powl.to_svg().unwrap().to_string()).unwrap();

        let _lpn = LabelledPetriNet::from(powl);

        // fs::write("lpn.svg", lpn.to_svg().unwrap().to_string()).unwrap();
    }

    #[test]
    fn powl_2_lpn_or_a_b() {
        let fin = fs::read_to_string("testfiles/or_a_b.powl").unwrap();
        let powl = fin.parse::<PartiallyOrderedWorkflowLanguage>().unwrap();

        // fs::write("powl.svg", powl.to_svg().unwrap().to_string()).unwrap();

        let _lpn = LabelledPetriNet::from(powl);

        // fs::write("lpn.svg", lpn.to_svg().unwrap().to_string()).unwrap();
    }
}
