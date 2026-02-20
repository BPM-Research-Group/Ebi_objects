#[cfg(any(test, feature = "testactivities"))]
use crate::activity_key::has_activity_key::TestActivityKey;
use crate::{
    ActivityKey, Graphable, Infoable, TranslateActivityKey,
    bpmn::objects::{BPMNElement, BPMNObject, BPMNProcess, MessageFlow},
    traits::graphable::{create_edge, create_gateway, create_place, create_transition},
};
use anyhow::{Result, anyhow};
use ebi_derive::ActivityKey;
use layout::{core::base::Orientation, topo::layout::VisualGraph};
use std::{
    collections::HashMap,
    fmt::{Display, Formatter},
    io::Write,
};

#[derive(Clone, ActivityKey)]
pub struct BusinessProcessModelAndNotation {
    pub(crate) activity_key: ActivityKey,

    pub collaboration_index: Option<usize>,
    pub collaboration_id: Option<String>,
    pub definitions_index: usize,
    pub definitions_id: String,

    /// pools
    pub processes: Vec<BPMNProcess>,

    /// messages
    pub message_flows: Vec<MessageFlow>,
}

impl Display for BusinessProcessModelAndNotation {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

impl Infoable for BusinessProcessModelAndNotation {
    fn info(&self, f: &mut impl Write) -> Result<()> {
        todo!()
    }
}

impl Graphable for BusinessProcessModelAndNotation {
    fn to_dot(&self) -> Result<VisualGraph> {
        let mut graph = VisualGraph::new(Orientation::LeftToRight);

        let mut object_2_node = HashMap::new();
        for process in &self.processes {
            //add nodes
            for element in &process.elements {
                let node = match element {
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
                    BPMNElement::ExclusiveGateway { .. } => create_gateway(&mut graph, "x"),
                    BPMNElement::InclusiveGateway { .. } => create_gateway(&mut graph, "o"),
                    BPMNElement::Task { activity, .. } => create_transition(
                        &mut graph,
                        self.activity_key.deprocess_activity(activity),
                        "",
                    ),
                };
                object_2_node.insert(element.index(), node);
            }

            //add edges
            for sequence_flow in &process.sequence_flows {
                let from = object_2_node
                    .get(&sequence_flow.source_element_index)
                    .ok_or_else(|| anyhow!("node not found"))?;
                let to = object_2_node
                    .get(&sequence_flow.target_element_index)
                    .ok_or_else(|| anyhow!("node not found"))?;

                create_edge(&mut graph, from, to, "");
            }
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

impl TranslateActivityKey for BusinessProcessModelAndNotation {
    fn translate_using_activity_key(&mut self, to_activity_key: &mut ActivityKey) {
        todo!()
    }
}

#[cfg(any(test, feature = "testactivities"))]
impl TestActivityKey for BusinessProcessModelAndNotation {
    fn test_activity_key(&self) {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use crate::{BusinessProcessModelAndNotation, Graphable};
    use std::fs::{self};

    #[test]
    fn bpmn_pool_import() {
        let fin = fs::read_to_string("testfiles/model-pool.bpmn").unwrap();
        let bpmn = fin.parse::<BusinessProcessModelAndNotation>().unwrap();

        let dot = bpmn.to_dot().unwrap();
    }
}
