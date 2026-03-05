use crate::{
    DeterministicFiniteAutomaton, DirectlyFollowsGraph, DirectlyFollowsModel, LabelledPetriNet,
    PetriNetMarkupLanguage, ProcessTree, ProcessTreeMarkupLanguage,
    StochasticDeterministicFiniteAutomaton, StochasticDirectlyFollowsModel,
    StochasticLabelledPetriNet, StochasticNondeterministicFiniteAutomaton, StochasticProcessTree,
};
use anyhow::{Error, Result, anyhow};
use ebi_bpmn::{
    BusinessProcessModelAndNotation,
    creator::{BPMNCreator, EndEventType, GatewayType, StartEventType},
};

impl TryFrom<LabelledPetriNet> for BusinessProcessModelAndNotation {
    type Error = Error;
    fn try_from(value: LabelledPetriNet) -> Result<Self> {
        let mut c = BPMNCreator::new_with_activity_key(value.activity_key.clone());
        let parent = c.add_process();

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
                        c.add_sequence_flow(parent, inp, out)?;
                        c.add_sequence_flow(parent, out, inp)?;
                        additional_start_elements.push(inp);
                        (inp, out)
                    }
                    (Card::Zero, Some(activity)) => {
                        let inp = c.add_gateway(parent, GatewayType::Exclusive)?;
                        let task = c.add_task(parent, activity)?;
                        let out = c.add_gateway(parent, GatewayType::Parallel)?;
                        c.add_sequence_flow(parent, inp, task)?;
                        c.add_sequence_flow(parent, task, out)?;
                        c.add_sequence_flow(parent, out, inp)?;
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
                        c.add_sequence_flow(parent, inp, out)?;
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
                    c.add_sequence_flow(parent, input_gateway, input_element)?;
                }
            }

            //connect outgoing edges
            match aout {
                Card::Zero => {
                    //there no outgoing edges of this transition; add an end event
                    let end_event = c.add_end_event(parent, EndEventType::None)?;
                    c.add_sequence_flow(parent, output_element, end_event)?;
                },
                Card::One | Card::Multiple => {
                    //there are outgoing edges; connect them
                    for (output_place, cardinality) in value.transition2output_places[transition]
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
                            c.add_sequence_flow(parent, output_element, output_gateway)?;
                        }
                    }
                }
            }
        }

        //add start element(s)
        let start_element = {
            let start_event = c.add_start_event(parent, StartEventType::None)?;
            if value.initial_marking.get_place2token().iter().sum::<u64>()
                + additional_start_elements.len() as u64
                == 1
            {
                //no additional gateway necessary
                start_event
            } else {
                //add an and gateway to start the process
                let and_gateway = c.add_gateway(parent, GatewayType::Parallel)?;
                c.add_sequence_flow(parent, start_event, and_gateway)?;
                and_gateway
            }
        };

        //connect start element
        for (place, cardinality) in value.initial_marking.get_place2token().iter().enumerate() {
            if cardinality > &1 {
                return Err(anyhow!(
                    "Non-safe nets are not supported, as these would lead to deadlocks in the translated BPMN model."
                ));
            } else if cardinality > &0 {
                let xor_gateway = place_2_xor_gateway[place];
                c.add_sequence_flow(parent, start_element, xor_gateway)?;
            }
        }
        for additional_start_element in additional_start_elements {
            c.add_sequence_flow(parent, start_element, additional_start_element)?;
        }

        c.to_bpmn()
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

        let _bpmn = BusinessProcessModelAndNotation::try_from(slpn).unwrap();
    }
}
