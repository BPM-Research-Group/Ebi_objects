use crate::{
    DeterministicFiniteAutomaton, DirectlyFollowsGraph, DirectlyFollowsModel, LabelledPetriNet, PetriNetMarkupLanguage, ProcessTree, ProcessTreeMarkupLanguage, StochasticDeterministicFiniteAutomaton, StochasticDirectlyFollowsModel, StochasticLabelledPetriNet, StochasticNondeterministicFiniteAutomaton, StochasticProcessTree
};
use ebi_bpmn::{
    BusinessProcessModelAndNotation,
    creator::{BPMNCreator, GatewayType},
};

impl From<LabelledPetriNet> for BusinessProcessModelAndNotation {
    fn from(value: LabelledPetriNet) -> Self {
        let mut c = BPMNCreator::new_with_activity_key(value.activity_key.clone());
        let process = c.add_process();

        //transform places to xor gateways
        let mut place_2_xor_gateway = vec![];
        for _ in 0..value.get_number_of_places() {
            place_2_xor_gateway.push(c.add_gateway_unchecked(process, GatewayType::Exclusive));
        }

        //transform transitions to input and output elements
        for transition in 0..value.get_number_of_transitions() {
            //store whether there are multiple input or output arcs
            let out_1 = value.transition2output_places[transition].len() == 1
                && value.transition2output_places_cardinality[transition][0] == 1;
            let in_1 = value.transition2input_places[transition].len() == 1
                && value.transition2input_places_cardinality[transition][0] == 1;

            if let Some(activity) = value.get_transition_label(transition) {
                //labelled transition
                let task = c.add_task_unchecked(process, activity);

                //incoming sequence flows
                if in_1 {
                    //only a single incoming arc -> no need for a gateway
                    let pre_place =
                        place_2_xor_gateway[value.transition2input_places[transition][0]];
                    c.add_sequence_flow_unchecked(process, pre_place, task);
                } else {
                    //multiple incoming arcs -> need a parallel gateway
                    let gateway = c.add_gateway_unchecked(process, GatewayType::Parallel);
                    for (output_place, cardinality) in value.transition2output_places[transition]
                        .iter()
                        .zip(value.transition2output_places_cardinality[transition].iter())
                    {
                        let output_gateway = place_2_xor_gateway[*output_place];
                        for _ in 0..*cardinality {
                            c.add_sequence_flow_unchecked(process, gateway, output_gateway);
                        }
                    }
                }

                //outgoing sequence flows
                for (output_place, cardinality) in value.transition2output_places[transition]
                    .iter()
                    .zip(value.transition2output_places_cardinality[transition].iter())
                {
                    let output_gateway = place_2_xor_gateway[*output_place];
                    for _ in 0..*cardinality {
                        c.add_sequence_flow_unchecked(process, task, output_gateway);
                    }
                }
            } else {
                //silent transition
                match (in_1, out_1) {
                    (true, true) => {
                        //single input, single output arc

                        //no gateway, just sequence flow
                        let pre_place =
                            place_2_xor_gateway[value.transition2input_places[transition][0]];
                        let post_place =
                            place_2_xor_gateway[value.transition2output_places[transition][0]];
                        c.add_sequence_flow_unchecked(process, pre_place, post_place);
                    }
                    (true, false) => {
                        let gateway = c.add_gateway_unchecked(process, GatewayType::Parallel);

                        //incoming sequence flows
                        for (input_place, cardinality) in value.transition2input_places[transition]
                            .iter()
                            .zip(value.transition2input_places_cardinality[transition].iter())
                        {
                            let input_gateway = place_2_xor_gateway[*input_place];
                            for _ in 0..*cardinality {
                                c.add_sequence_flow_unchecked(process, input_gateway, gateway);
                            }
                        }

                        //outgoing sequence flows
                        for (output_place, cardinality) in value.transition2output_places
                            [transition]
                            .iter()
                            .zip(value.transition2output_places_cardinality[transition].iter())
                        {
                            let output_gateway = place_2_xor_gateway[*output_place];
                            for _ in 0..*cardinality {
                                c.add_sequence_flow_unchecked(process, gateway, output_gateway);
                            }
                        }
                    }
                    (false, true) => {
                        let gateway = c.add_gateway_unchecked(process, GatewayType::Exclusive);

                        //incoming sequence flows
                        for (input_place, cardinality) in value.transition2input_places[transition]
                            .iter()
                            .zip(value.transition2input_places_cardinality[transition].iter())
                        {
                            let input_gateway = place_2_xor_gateway[*input_place];
                            for _ in 0..*cardinality {
                                c.add_sequence_flow_unchecked(process, input_gateway, gateway);
                            }
                        }

                        //outgoing sequence flows
                        for (output_place, cardinality) in value.transition2output_places
                            [transition]
                            .iter()
                            .zip(value.transition2output_places_cardinality[transition].iter())
                        {
                            let output_gateway = place_2_xor_gateway[*output_place];
                            for _ in 0..*cardinality {
                                c.add_sequence_flow_unchecked(process, gateway, output_gateway);
                            }
                        }
                    }
                    (false, false) => {
                        //multiple inputs, multiple outputs
                        let input = c.add_gateway_unchecked(process, GatewayType::Exclusive);
                        let output = c.add_gateway_unchecked(process, GatewayType::Parallel);
                        c.add_sequence_flow_unchecked(process, input, output);

                        //incoming sequence flows
                        for (input_place, cardinality) in value.transition2input_places[transition]
                            .iter()
                            .zip(value.transition2input_places_cardinality[transition].iter())
                        {
                            let input_gateway = place_2_xor_gateway[*input_place];
                            for _ in 0..*cardinality {
                                c.add_sequence_flow_unchecked(process, input_gateway, input);
                            }
                        }

                        //outgoing sequence flows
                        for (output_place, cardinality) in value.transition2output_places
                            [transition]
                            .iter()
                            .zip(value.transition2output_places_cardinality[transition].iter())
                        {
                            let output_gateway = place_2_xor_gateway[*output_place];
                            for _ in 0..*cardinality {
                                c.add_sequence_flow_unchecked(process, output, output_gateway);
                            }
                        }
                    }
                }
            }
        }

        //structural correctness guaranteed
        c.to_bpmn().unwrap()
    }
}

macro_rules! via_lpn {
    ($t:ident) => {
        impl From<$t> for BusinessProcessModelAndNotation {
            fn from(value: $t) -> Self {
                LabelledPetriNet::from(value).into()
            }
        }
    };
}

via_lpn!(DeterministicFiniteAutomaton);
via_lpn!(DirectlyFollowsGraph);
via_lpn!(DirectlyFollowsModel);
via_lpn!(StochasticDirectlyFollowsModel);
via_lpn!(ProcessTree);
via_lpn!(PetriNetMarkupLanguage);
via_lpn!(ProcessTreeMarkupLanguage);
via_lpn!(StochasticProcessTree);
via_lpn!(StochasticLabelledPetriNet);
via_lpn!(StochasticDeterministicFiniteAutomaton);
via_lpn!(StochasticNondeterministicFiniteAutomaton);

#[cfg(test)]
mod tests {
    use crate::StochasticLabelledPetriNet;
    use ebi_bpmn::BusinessProcessModelAndNotation;
    use std::fs;

    #[test]
    fn slpn_2_bpmn() {
        let fin = fs::read_to_string("testfiles/a-aa-bb.slpn").unwrap();
        let slpn = fin.parse::<StochasticLabelledPetriNet>().unwrap();

        let _bpmn = BusinessProcessModelAndNotation::from(slpn);
    }
}
