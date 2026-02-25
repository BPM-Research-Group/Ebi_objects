use anyhow::{Result, anyhow};
use ebi_activity_key::HasActivityKey;
use ebi_bpmn::{
    BusinessProcessModelAndNotation, element::BPMNElement, elements::task::BPMNTask,
    objects_objectable::BPMNObject,
};
use layout::{core::base::Orientation, topo::layout::VisualGraph};
use std::{collections::HashMap, io::Write};

use crate::{
    Graphable, Infoable,
    traits::graphable::{create_edge, create_gateway, create_place, create_transition},
};

impl Infoable for BusinessProcessModelAndNotation {
    fn info(&self, f: &mut impl Write) -> Result<()> {
        writeln!(f, "Number of processes (pools)\t{}", self.elements.len())?;
        writeln!(f, "Number of elements\t\t{}", self.number_of_elements())?;
        writeln!(f, "Number of message flows\t\t{}", self.message_flows.len())?;
        writeln!(
            f,
            "Number of activities\t\t{}",
            self.activity_key().get_number_of_activities()
        )?;

        writeln!(f, "")?;
        self.activity_key().info(f)?;

        Ok(writeln!(f, "")?)
    }
}

impl Graphable for BusinessProcessModelAndNotation {
    fn to_dot(&self) -> Result<VisualGraph> {
        let mut graph = VisualGraph::new(Orientation::LeftToRight);

        let mut object_2_node = HashMap::new();
        for element in &self.all_elements_ref() {
            //add nodes

            let node = match element {
                BPMNElement::CollapsedPool(_) => create_place(&mut graph, "cp"),
                BPMNElement::EndEvent { .. } => create_place(&mut graph, "e"),
                BPMNElement::MessageEndEvent { .. } => create_place(&mut graph, "me"),
                BPMNElement::StartEvent { .. } => create_place(&mut graph, "s"),
                BPMNElement::MessageStartEvent { .. } => create_place(&mut graph, "ms"),
                BPMNElement::IntermediateCatchEvent { .. } => create_place(&mut graph, "ci"),
                BPMNElement::MessageIntermediateCatchEvent { .. } => {
                    create_place(&mut graph, "mci")
                }
                BPMNElement::IntermediateThrowEvent { .. } => create_place(&mut graph, "ti"),
                BPMNElement::MessageIntermediateThrowEvent { .. } => {
                    create_place(&mut graph, "mti")
                }
                BPMNElement::CollapsedSubProcess { .. } => create_transition(&mut graph, "csp", ""),
                BPMNElement::ExpandedSubProcess { .. } => create_transition(&mut graph, "esp", ""),
                BPMNElement::ExclusiveGateway { .. } => create_gateway(&mut graph, "x"),
                BPMNElement::InclusiveGateway { .. } => create_gateway(&mut graph, "o"),
                BPMNElement::ParallelGateway { .. } => create_gateway(&mut graph, "+"),
                BPMNElement::Process(_) => create_place(&mut graph, "p"),
                BPMNElement::EventBasedGateway { .. } => create_gateway(&mut graph, "e"),
                BPMNElement::Task(BPMNTask { activity, .. }) => create_transition(
                    &mut graph,
                    self.activity_key().deprocess_activity(activity),
                    "",
                ),
            };
            object_2_node.insert(element.index(), node);
        }

        //add edges
        for sequence_flow in &self.sequence_flows {
            let from = object_2_node
                .get(&sequence_flow.source_index)
                .ok_or_else(|| anyhow!("node not found"))?;
            let to = object_2_node
                .get(&sequence_flow.target_index)
                .ok_or_else(|| anyhow!("node not found"))?;

            create_edge(&mut graph, from, to, "");
        }

        for message_flow in &self.message_flows {
            let from = object_2_node
                .get(&message_flow.source_element_index)
                .ok_or_else(|| anyhow!("node not found"))?;
            let to = object_2_node
                .get(&message_flow.target_element_index)
                .ok_or_else(|| anyhow!("node not found"))?;

            create_edge(&mut graph, from, to, "m");
        }

        Ok(graph)
    }
}

#[cfg(test)]
mod tests {
    use ebi_activity_key::{HasActivityKey, TestActivityKey};

    use crate::{BusinessProcessModelAndNotation, Graphable, Infoable, TranslateActivityKey};
    use std::fs::{self};

    #[test]
    fn bpmn_pool_graphable() {
        let fin = fs::read_to_string("testfiles/model-lanes.bpmn").unwrap();
        let bpmn = fin.parse::<BusinessProcessModelAndNotation>().unwrap();

        bpmn.to_dot().unwrap();
    }

    #[test]
    fn bpmn_pool_translate() {
        let fin = fs::read_to_string("testfiles/model-lanes.bpmn").unwrap();
        let mut bpmn = fin.parse::<BusinessProcessModelAndNotation>().unwrap();

        bpmn.test_activity_key();

        let fin2 = fs::read_to_string("testfiles/model.bpmn").unwrap();
        let mut bpmn2 = fin2.parse::<BusinessProcessModelAndNotation>().unwrap();

        bpmn.translate_using_activity_key(&mut bpmn2.activity_key_mut());

        bpmn.test_activity_key();
    }

    #[test]
    fn bpmn_pool_infoable() {
        let fin = fs::read_to_string("testfiles/model-lanes.bpmn").unwrap();
        let bpmn = fin.parse::<BusinessProcessModelAndNotation>().unwrap();

        let mut f = vec![];
        bpmn.info(&mut f).unwrap();
        println!("{}", String::from_utf8_lossy(&f));
    }
}
