use std::collections::{HashMap, hash_map::Entry};

use crate::{
    DeterministicFiniteAutomaton, DirectlyFollowsGraph, DirectlyFollowsModel, LabelledPetriNet,
    PetriNetMarkupLanguage, ProcessTree, ProcessTreeMarkupLanguage,
    StochasticDeterministicFiniteAutomaton, StochasticDirectlyFollowsModel,
    StochasticLabelledPetriNet, StochasticNondeterministicFiniteAutomaton, StochasticProcessTree,
    ebi_objects::{
        partially_ordered_workflow_language::{PartiallyOrderedWorkflowLanguage, PowlNode},
        process_tree::{Node, Operator},
    },
};
use ebi_activity_key::Activity;
use ebi_bpmn::ebi_arithmetic::anyhow::{Error, Result, anyhow};
use ebi_bpmn::{
    BPMNCreator, BusinessProcessModelAndNotation, Container, EndEventType, GatewayType,
    GlobalIndex, StartEventType,
};

impl TryFrom<LabelledPetriNet> for BusinessProcessModelAndNotation {
    type Error = Error;
    fn try_from(value: LabelledPetriNet) -> Result<Self> {
        if let Some(initial_marking) = &value.initial_marking {
            let mut c = BPMNCreator::new_with_activity_key(value.activity_key.clone());
            let parent = c.add_process(None);

            //transform places to xor gateways
            let mut place_2_xor_gateway = vec![];
            for place_index in 0..value.get_number_of_places() {
                let is_input_of_transition = value
                    .transition2input_places
                    .iter()
                    .zip(value.transition2input_places_cardinality.iter())
                    .any(|(x, y)| {
                        x.iter()
                            .zip(y.iter())
                            .any(|(place, cardinality)| place_index == *place && cardinality > &0)
                    });

                if is_input_of_transition {
                    place_2_xor_gateway.push(c.add_gateway(parent, GatewayType::Exclusive)?);
                } else {
                    place_2_xor_gateway.push(c.add_end_event(parent, EndEventType::None)?);
                }
            }

            let mut additional_start_elements = vec![];

            enum Card {
                Zero,
                One,
                Multiple,
            }

            //transform transitions to input and output elements
            for transition in 0..value.get_number_of_transitions() {
                //store whether there are multiple input or output arcs
                let ain = if value.transition2input_places[transition].len() == 0 {
                    Card::Zero
                } else if value.transition2input_places[transition].len() == 1
                    && value.transition2input_places_cardinality[transition][0] == 1
                {
                    Card::One
                } else {
                    Card::Multiple
                };

                let aout = if value.transition2output_places[transition].len() == 0 {
                    Card::Zero
                } else if value.transition2output_places[transition].len() == 1
                    && value.transition2output_places_cardinality[transition][0] == 1
                {
                    Card::One
                } else {
                    Card::Multiple
                };

                // each transition is translated into an input and an output element
                let (input_element, output_element) =
                    match (ain, value.get_transition_label(transition)) {
                        (Card::Zero, None) => {
                            let inp = c.add_gateway(parent, GatewayType::Exclusive)?;
                            let out = c.add_gateway(parent, GatewayType::Parallel)?;
                            c.add_sequence_flow(inp, out)?;
                            c.add_sequence_flow(out, inp)?;
                            additional_start_elements.push(inp);
                            (inp, out)
                        }
                        (Card::Zero, Some(activity)) => {
                            let inp = c.add_gateway(parent, GatewayType::Exclusive)?;
                            let task = c.add_task(parent, activity)?;
                            let out = c.add_gateway(parent, GatewayType::Parallel)?;
                            c.add_sequence_flow(inp, task)?;
                            c.add_sequence_flow(task, out)?;
                            c.add_sequence_flow(out, inp)?;
                            additional_start_elements.push(inp);
                            (inp, out)
                        }
                        (Card::One, None) => {
                            let gateway = c.add_gateway(parent, GatewayType::Parallel)?;
                            (gateway, gateway)
                        }
                        (Card::One, Some(activity)) => {
                            let task = c.add_task(parent, activity)?;
                            (task, task)
                        }
                        (Card::Multiple, None) => {
                            let gateway = c.add_gateway(parent, GatewayType::Parallel)?;
                            (gateway, gateway)
                        }
                        (Card::Multiple, Some(activity)) => {
                            let inp = c.add_gateway(parent, GatewayType::Parallel)?;
                            let out = c.add_task(parent, activity)?;
                            c.add_sequence_flow(inp, out)?;
                            (inp, out)
                        }
                    };

                //connect incoming edges
                for (input_place, cardinality) in value.transition2input_places[transition]
                    .iter()
                    .zip(value.transition2input_places_cardinality[transition].iter())
                {
                    let input_gateway = place_2_xor_gateway[*input_place];
                    if cardinality > &1 {
                        return Err(anyhow!(
                            "Arc weights are not supported, as these would lead to deadlocks in the translated BPMN model."
                        ));
                    }
                    if cardinality > &0 {
                        c.add_sequence_flow(input_gateway, input_element)?;
                    }
                }

                //connect outgoing edges
                match aout {
                    Card::Zero => {
                        //there no outgoing edges of this transition; add an end event
                        let end_event = c.add_end_event(parent, EndEventType::None)?;
                        c.add_sequence_flow(output_element, end_event)?;
                    }
                    Card::One | Card::Multiple => {
                        //there are outgoing edges; connect them
                        for (output_place, cardinality) in value.transition2output_places
                            [transition]
                            .iter()
                            .zip(value.transition2output_places_cardinality[transition].iter())
                        {
                            let output_gateway = place_2_xor_gateway[*output_place];
                            if cardinality > &1 {
                                return Err(anyhow!(
                                    "Arc weights are not supported, as these would lead to deadlocks in the translated BPMN model."
                                ));
                            }
                            if cardinality > &0 {
                                c.add_sequence_flow(output_element, output_gateway)?;
                            }
                        }
                    }
                }
            }

            //add start element(s)
            let start_element = {
                let start_event = c.add_start_event(parent, StartEventType::None)?;
                if initial_marking.get_place2token().iter().sum::<u64>()
                    + additional_start_elements.len() as u64
                    == 1
                {
                    //no additional gateway necessary
                    start_event
                } else {
                    //add an and gateway to start the process
                    let and_gateway = c.add_gateway(parent, GatewayType::Parallel)?;
                    c.add_sequence_flow(start_event, and_gateway)?;
                    and_gateway
                }
            };

            //connect start element
            for (place, cardinality) in initial_marking.get_place2token().iter().enumerate() {
                if cardinality > &1 {
                    return Err(anyhow!(
                        "Non-safe nets are not supported, as these would lead to deadlocks in the translated BPMN model."
                    ));
                } else if cardinality > &0 {
                    let xor_gateway = place_2_xor_gateway[place];
                    c.add_sequence_flow(start_element, xor_gateway)?;
                }
            }
            for additional_start_element in additional_start_elements {
                c.add_sequence_flow(start_element, additional_start_element)?;
            }

            c.to_bpmn()
        } else {
            return Err(anyhow!(
                "The LPN has the empty language. BPMN cannot represent this model."
            ));
        }
    }
}

macro_rules! via_lpn {
    ($t:ident) => {
        impl TryFrom<$t> for BusinessProcessModelAndNotation {
            type Error = Error;
            fn try_from(value: $t) -> Result<Self> {
                LabelledPetriNet::from(value).try_into()
            }
        }
    };
}

via_lpn!(DeterministicFiniteAutomaton);
via_lpn!(DirectlyFollowsGraph);
via_lpn!(DirectlyFollowsModel);
via_lpn!(StochasticDirectlyFollowsModel);
via_lpn!(PetriNetMarkupLanguage);
via_lpn!(StochasticLabelledPetriNet);
via_lpn!(StochasticDeterministicFiniteAutomaton);
via_lpn!(StochasticNondeterministicFiniteAutomaton);

impl From<ProcessTree> for BusinessProcessModelAndNotation {
    fn from(value: ProcessTree) -> Self {
        let mut c = BPMNCreator::new_with_activity_key(value.activity_key.clone());
        let parent = c.add_process(None);

        let source = c.add_start_event_unchecked(parent, StartEventType::None);
        let sink = c.add_end_event_unchecked(parent, EndEventType::None);
        let root = value.root();
        if !transform_node(&value, root, source, sink, &mut c, parent) {
            //empty model
            BPMNCreator::new().to_bpmn().unwrap()
        } else {
            c.to_bpmn().unwrap()
        }
    }
}

fn transform_node(
    tree: &ProcessTree,
    node: usize,
    source: GlobalIndex,
    sink: GlobalIndex,
    c: &mut BPMNCreator,
    parent: Container,
) -> bool {
    match tree.get_node(node) {
        Some(Node::Tau) => {
            c.add_sequence_flow_unchecked(parent, source, sink);
            true
        }
        Some(Node::Activity(activity)) => {
            let task = c.add_task_unchecked(parent, *activity);
            c.add_sequence_flow_unchecked(parent, source, task);
            c.add_sequence_flow_unchecked(parent, task, sink);
            true
        }
        Some(Node::Operator(Operator::Concurrent, _)) => {
            let sub_source = c.add_gateway_unchecked(parent, GatewayType::Parallel);
            let sub_sink = c.add_gateway_unchecked(parent, GatewayType::Parallel);
            c.add_sequence_flow_unchecked(parent, source, sub_source);
            c.add_sequence_flow_unchecked(parent, sub_sink, sink);
            for child in tree.get_children(node) {
                transform_node(tree, child, sub_source, sub_sink, c, parent);
            }
            true
        }
        Some(Node::Operator(Operator::Xor, _)) => {
            let sub_source = c.add_gateway_unchecked(parent, GatewayType::Exclusive);
            let sub_sink = c.add_gateway_unchecked(parent, GatewayType::Exclusive);
            c.add_sequence_flow_unchecked(parent, source, sub_source);
            c.add_sequence_flow_unchecked(parent, sub_sink, sink);
            for child in tree.get_children(node) {
                transform_node(tree, child, sub_source, sub_sink, c, parent);
            }
            true
        }
        Some(Node::Operator(Operator::Or, _)) => {
            let sub_source = c.add_gateway_unchecked(parent, GatewayType::Inclusive);
            let sub_sink = c.add_gateway_unchecked(parent, GatewayType::Inclusive);
            c.add_sequence_flow_unchecked(parent, source, sub_source);
            c.add_sequence_flow_unchecked(parent, sub_sink, sink);
            for child in tree.get_children(node) {
                transform_node(tree, child, sub_source, sub_sink, c, parent);
            }
            true
        }
        Some(Node::Operator(Operator::Loop, _)) => {
            let sub_source = c.add_gateway_unchecked(parent, GatewayType::Exclusive);
            let sub_sink = c.add_gateway_unchecked(parent, GatewayType::Exclusive);
            c.add_sequence_flow_unchecked(parent, source, sub_source);
            c.add_sequence_flow_unchecked(parent, sub_sink, sink);
            let mut it = tree.get_children(node);
            if let Some(child) = it.next() {
                transform_node(tree, child, sub_source, sub_sink, c, parent);
            }
            while let Some(child) = it.next() {
                transform_node(tree, child, sub_sink, sub_source, c, parent);
            }
            true
        }
        Some(Node::Operator(Operator::Sequence, _)) => {
            let mut it = tree.get_children(node);
            let mut sub_sink = c.add_gateway_unchecked(parent, GatewayType::Exclusive);
            if let Some(child) = it.next() {
                transform_node(tree, child, source, sub_sink, c, parent);
            }
            while let Some(child) = it.next() {
                let sub_source = sub_sink;
                sub_sink = c.add_gateway_unchecked(parent, GatewayType::Exclusive);
                transform_node(tree, child, sub_source, sub_sink, c, parent);
            }
            c.add_sequence_flow_unchecked(parent, sub_sink, sink);
            true
        }
        Some(Node::Operator(Operator::Interleaved, _)) => {
            let children = tree.get_children(node).collect::<Vec<_>>();
            let mut children_left_2_gateway = HashMap::new();
            children_left_2_gateway.insert(vec![], sink);
            children_left_2_gateway.insert(children.clone(), source);

            let mut queue = vec![];
            queue.push(children);
            while let Some(children_left) = queue.pop() {
                let sub_source = *children_left_2_gateway.get(&children_left).unwrap();
                for (i, child) in children_left.iter().enumerate() {
                    //remove one child
                    let mut sub_children_left = children_left.clone();
                    sub_children_left.remove(i);

                    //fetch a sub-sink or create a new one
                    let sub_sink = match children_left_2_gateway.entry(sub_children_left.clone()) {
                        Entry::Occupied(occupied_entry) => *occupied_entry.get(),
                        Entry::Vacant(vacant_entry) => {
                            //new, add to queue
                            queue.push(sub_children_left);
                            let sub_sink = c.add_gateway_unchecked(parent, GatewayType::Exclusive);
                            vacant_entry.insert(sub_sink);
                            sub_sink
                        }
                    };

                    transform_node(tree, *child, sub_source, sub_sink, c, parent);
                }
            }
            true
        }
        None => false,
    }
}

impl From<StochasticProcessTree> for BusinessProcessModelAndNotation {
    fn from(value: StochasticProcessTree) -> Self {
        ProcessTree::from(value).into()
    }
}

impl From<ProcessTreeMarkupLanguage> for BusinessProcessModelAndNotation {
    fn from(value: ProcessTreeMarkupLanguage) -> Self {
        ProcessTree::from(value).into()
    }
}

impl From<PartiallyOrderedWorkflowLanguage> for BusinessProcessModelAndNotation {
    fn from(value: PartiallyOrderedWorkflowLanguage) -> Self {
        log::info!("convert partially ordered workflow language to LPN");
        let mut result = BPMNCreator::new_with_activity_key(value.activity_key.clone());
        if !value.tree.is_empty() {
            let process = result.add_process(None);

            let source = result.add_start_event_unchecked(process, StartEventType::None);
            let sink = result.add_end_event_unchecked(process, EndEventType::None);

            powl_node_multiplicity(&value, &mut result, 0, process, source, sink);
        }
        result.to_bpmn_unchecked()
    }
}

fn powl_node_multiplicity(
    powl: &PartiallyOrderedWorkflowLanguage,
    result: &mut BPMNCreator,
    node_index: usize,
    process: Container,
    source: GlobalIndex,
    sink: GlobalIndex,
) {
    //first, handle skippable and repeatable
    match powl.tree[node_index] {
        PowlNode::Activity {
            skippable,
            repeatable,
            ..
        }
        | PowlNode::PartialOrder {
            skippable,
            repeatable,
            ..
        }
        | PowlNode::ChoiceGraph {
            skippable,
            repeatable,
            ..
        } => {
            let (new_source, new_sink) = match (skippable, repeatable) {
                (true, true) => {
                    //zero or more times
                    let new_sink: GlobalIndex =
                        result.add_gateway_unchecked(process, GatewayType::Exclusive);
                    let new_source = result.add_gateway_unchecked(process, GatewayType::Exclusive);

                    result.add_sequence_flow_unchecked(process, source, new_sink);
                    result.add_sequence_flow_unchecked(process, new_source, sink);
                    result.add_sequence_flow_unchecked(process, new_sink, new_source);

                    (new_source, new_sink)
                }
                (true, false) => {
                    //zero or one times
                    let new_source = result.add_gateway_unchecked(process, GatewayType::Exclusive);
                    let new_sink = result.add_gateway_unchecked(process, GatewayType::Exclusive);

                    result.add_sequence_flow_unchecked(process, source, new_source);
                    result.add_sequence_flow_unchecked(process, new_sink, sink);
                    result.add_sequence_flow_unchecked(process, new_source, new_sink);

                    (new_source, new_sink)
                }
                (false, true) => {
                    //one or more times
                    let new_source = result.add_gateway_unchecked(process, GatewayType::Exclusive);
                    let new_sink = result.add_gateway_unchecked(process, GatewayType::Exclusive);

                    result.add_sequence_flow_unchecked(process, source, new_source);
                    result.add_sequence_flow_unchecked(process, new_sink, sink);
                    result.add_sequence_flow_unchecked(process, new_sink, new_source);

                    (new_source, new_sink)
                }
                (false, false) => {
                    //once
                    (source, sink)
                }
            };

            powl_node_2_bpmn(powl, result, node_index, process, new_source, new_sink);
        }
    }
}

fn powl_node_2_bpmn(
    powl: &PartiallyOrderedWorkflowLanguage,
    result: &mut BPMNCreator,
    node_index: usize,
    process: Container,
    source: GlobalIndex,
    sink: GlobalIndex,
) {
    match &powl.tree[node_index] {
        PowlNode::Activity { activity, .. } => {
            powl_activity_2_bpmn(activity, result, process, source, sink)
        }
        PowlNode::PartialOrder {
            edges,
            number_of_children,
            ..
        } => powl_partial_order_2_bpmn(
            powl,
            result,
            node_index,
            edges,
            *number_of_children,
            process,
            source,
            sink,
        ),
        PowlNode::ChoiceGraph {
            edges,
            start_children: start_nodes,
            end_children: end_nodes,
            ..
        } => powl_choice_graph_2_bpmn(
            powl,
            result,
            node_index,
            &edges,
            &start_nodes,
            &end_nodes,
            process,
            source,
            sink,
        ),
    }
}

fn powl_activity_2_bpmn(
    activity: &Option<Activity>,
    result: &mut BPMNCreator,
    process: Container,
    source: GlobalIndex,
    sink: GlobalIndex,
) {
    if let Some(activity) = activity {
        //task
        let task = result.add_task_unchecked(process, *activity);
        result.add_sequence_flow_unchecked(process, source, task);
        result.add_sequence_flow_unchecked(process, task, sink);
    } else {
        //silent
        result.add_sequence_flow_unchecked(process, source, sink);
    }
}

fn powl_partial_order_2_bpmn(
    powl: &PartiallyOrderedWorkflowLanguage,
    result: &mut BPMNCreator,
    node_index: usize,
    edges: &[(usize, usize)],
    number_of_children: usize,
    process: Container,
    source: GlobalIndex,
    sink: GlobalIndex,
) {
    //children
    let index_2_handles = powl
        .get_children(node_index)
        .map(|child_index| {
            let child_source = result.add_gateway_unchecked(process, GatewayType::Parallel);
            let child_sink = result.add_gateway_unchecked(process, GatewayType::Parallel);

            powl_node_multiplicity(powl, result, child_index, process, child_source, child_sink);

            (child_source, child_sink)
        })
        .collect::<Vec<_>>();

    let mut is_start = vec![true; number_of_children];
    let mut is_end = vec![true; number_of_children];

    //edges
    for (source_index, target_index) in edges {
        let entry = index_2_handles[*source_index].1;
        let exit = index_2_handles[*target_index].0;

        result.add_sequence_flow_unchecked(process, entry, exit);

        is_start[*target_index] = false;
        is_end[*source_index] = false;
    }

    //start
    let entry = result.add_gateway_unchecked(process, GatewayType::Parallel);
    result.add_sequence_flow_unchecked(process, source, entry);
    for (target_index, x) in is_start.into_iter().enumerate() {
        if x {
            let target = index_2_handles[target_index].0;
            result.add_sequence_flow_unchecked(process, entry, target);
        }
    }

    let exit = result.add_gateway_unchecked(process, GatewayType::Parallel);
    result.add_sequence_flow_unchecked(process, exit, sink);
    for (source_index, x) in is_end.into_iter().enumerate() {
        if x {
            let source = index_2_handles[source_index].1;
            result.add_sequence_flow_unchecked(process, source, exit);
        }
    }
}

fn powl_choice_graph_2_bpmn(
    powl: &PartiallyOrderedWorkflowLanguage,
    result: &mut BPMNCreator,
    node_index: usize,
    edges: &[(usize, usize)],
    start_nodes: &[usize],
    end_nodes: &[usize],
    process: Container,
    source: GlobalIndex,
    sink: GlobalIndex,
) {
    //children
    let index_2_handles = powl
        .get_children(node_index)
        .map(|child_index| {
            let child_source = result.add_gateway_unchecked(process, GatewayType::Exclusive);
            let child_sink = result.add_gateway_unchecked(process, GatewayType::Exclusive);

            powl_node_multiplicity(powl, result, child_index, process, child_source, child_sink);

            (child_source, child_sink)
        })
        .collect::<Vec<_>>();

    //edges
    for (source_index, target_index) in edges {
        let source = index_2_handles[*source_index].1;
        let target = index_2_handles[*target_index].0;
        result.add_sequence_flow_unchecked(process, source, target);
    }

    //start edges
    for target_index in start_nodes {
        let target = index_2_handles[*target_index].0;
        result.add_sequence_flow_unchecked(process, source, target);
    }

    //end edges
    for source_index in end_nodes {
        let source = index_2_handles[*source_index].1;
        result.add_sequence_flow_unchecked(process, source, sink);
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ProcessTree, StochasticLabelledPetriNet,
        ebi_objects::partially_ordered_workflow_language::PartiallyOrderedWorkflowLanguage,
    };
    use ebi_bpmn::BusinessProcessModelAndNotation;
    use std::fs;

    #[test]
    fn slpn_2_bpmn() {
        let fin = fs::read_to_string("testfiles/a-aa-bb.slpn").unwrap();
        let slpn = fin.parse::<StochasticLabelledPetriNet>().unwrap();

        let _bpmn = BusinessProcessModelAndNotation::try_from(slpn).unwrap();
    }

    #[test]
    fn ptree_2_bpmn() {
        let fin = fs::read_to_string("testfiles/empty_2.ptree").unwrap();
        let slpn = fin.parse::<ProcessTree>().unwrap();

        let _bpmn = BusinessProcessModelAndNotation::from(slpn);
    }

    #[test]
    fn powl_2_bpmn_a() {
        let fin = fs::read_to_string("testfiles/a.powl").unwrap();
        let powl = fin.parse::<PartiallyOrderedWorkflowLanguage>().unwrap();

        // fs::write("powl.svg", powl.to_svg().unwrap().to_string()).unwrap();

        let _bpmn = BusinessProcessModelAndNotation::from(powl);

        // fs::write("lpn.svg", bpmn.to_svg().unwrap().to_string()).unwrap();
    }

    #[test]
    fn powl_2_bpmn_and_a_b() {
        let fin = fs::read_to_string("testfiles/and_a_b.powl").unwrap();
        let powl = fin.parse::<PartiallyOrderedWorkflowLanguage>().unwrap();

        // fs::write("powl.svg", powl.to_svg().unwrap().to_string()).unwrap();

        let _bpmn = BusinessProcessModelAndNotation::from(powl);

        // fs::write("lpn.svg", bpmn.to_svg().unwrap().to_string()).unwrap();
    }

    #[test]
    fn powl_2_bpmn_or_a_b() {
        let fin = fs::read_to_string("testfiles/or_a_b.powl").unwrap();
        let powl = fin.parse::<PartiallyOrderedWorkflowLanguage>().unwrap();

        // fs::write("powl.svg", powl.to_svg().unwrap().to_string()).unwrap();

        let _bpmn = BusinessProcessModelAndNotation::from(powl);

        // fs::write("lpn.svg", bpmn.to_svg().unwrap().to_string()).unwrap();
    }

    #[test]
    fn powl_2_bpmn_skippable_repeatable() {
        let fin = fs::read_to_string("testfiles/skippable_repeatable.powl").unwrap();
        let powl = fin.parse::<PartiallyOrderedWorkflowLanguage>().unwrap();

        // fs::write("powl.svg", powl.to_svg().unwrap().to_string()).unwrap();

        let _bpmn = BusinessProcessModelAndNotation::from(powl);

        // fs::write("lpn.svg", bpmn.to_svg().unwrap().to_string()).unwrap();
    }
}
