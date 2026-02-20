#[cfg(any(test, feature = "testactivities"))]
use crate::activity_key::has_activity_key::TestActivityKey;
use crate::{
    ActivityKey, ActivityKeyTranslator, Graphable, HasActivityKey, Infoable, TranslateActivityKey,
    bpmn::{
        element::BPMNElement, message_flow::MessageFlow, objects::BPMNObject, process::BPMNProcess,
    },
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

impl BusinessProcessModelAndNotation {
    fn number_of_elements(&self) -> usize {
        self.processes
            .iter()
            .map(|process| process.all_elements_ref().len())
            .sum()
    }
}

impl Display for BusinessProcessModelAndNotation {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "BPMN model with {} elements", self.number_of_elements())
    }
}

impl Infoable for BusinessProcessModelAndNotation {
    fn info(&self, f: &mut impl Write) -> Result<()> {
        writeln!(f, "Number of processes (pools)\t{}", self.processes.len())?;
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
                    BPMNElement::ParallelGateway { .. } => create_gateway(&mut graph, "+"),
                    BPMNElement::EventBasedGateway { .. } => create_gateway(&mut graph, "e"),
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
        let translator = ActivityKeyTranslator::new(&self.activity_key, to_activity_key);

        for process_rank in 0..self.processes.len() {
            //gather indices of elements
            let mut indices = vec![];
            for element in self.processes[process_rank].all_elements_ref() {
                if let BPMNElement::Task { index, .. } = element {
                    indices.push(*index);
                }
            }

            //adjust activities
            for index in indices {
                if let Some(BPMNElement::Task { activity, .. }) =
                    self.processes[process_rank].element_mut(index)
                {
                    *activity = translator.translate_activity(&activity);
                } else {
                    unreachable!()
                }
            }
        }
        self.activity_key = to_activity_key.clone();
    }
}

#[cfg(any(test, feature = "testactivities"))]
impl TestActivityKey for BusinessProcessModelAndNotation {
    fn test_activity_key(&self) {
        for process in &self.processes {
            for element in process.all_elements_ref() {
                if let BPMNElement::Task { activity, .. } = element {
                    self.activity_key.assert_activity_is_of_key(activity);
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        BusinessProcessModelAndNotation, Graphable, Infoable, TranslateActivityKey,
        activity_key::has_activity_key::TestActivityKey,
    };
    use std::fs::{self};

    #[test]
    fn bpmn_pool_graphable() {
        let fin = fs::read_to_string("testfiles/model-pool.bpmn").unwrap();
        let bpmn = fin.parse::<BusinessProcessModelAndNotation>().unwrap();

        bpmn.to_dot().unwrap();
    }

    #[test]
    fn bpmn_pool_translate() {
        let fin = fs::read_to_string("testfiles/model-pool.bpmn").unwrap();
        let mut bpmn = fin.parse::<BusinessProcessModelAndNotation>().unwrap();

        bpmn.test_activity_key();

        let fin2 = fs::read_to_string("testfiles/model.bpmn").unwrap();
        let mut bpmn2 = fin2.parse::<BusinessProcessModelAndNotation>().unwrap();

        bpmn.translate_using_activity_key(&mut bpmn2.activity_key);

        bpmn.test_activity_key();
    }

    #[test]
    fn bpmn_pool_infoable() {
        let fin = fs::read_to_string("testfiles/model-pool.bpmn").unwrap();
        let bpmn = fin.parse::<BusinessProcessModelAndNotation>().unwrap();

        let mut f = vec![];
        bpmn.info(&mut f).unwrap();
        println!("{}", String::from_utf8_lossy(&f));
    }
}
