use crate::{
    BusinessProcessModelAndNotation,
    bpmn::{
        element::{BPMNElement, BPMNElementTrait},
        objects_objectable::{BPMNObject, EMPTY_FLOWS}, objects_transitionable::Transitionable,
    },
};
use anyhow::{Result, anyhow};

#[derive(Debug, Clone)]
pub struct BPMNExpandedSubProcess {
    pub(crate) index: usize,
    pub(crate) id: String,
    pub(crate) name: Option<String>,
    pub(crate) elements: Vec<BPMNElement>,
    pub(crate) incoming_sequence_flows: Vec<usize>,
    pub(crate) outgoing_sequence_flows: Vec<usize>,
}

impl BPMNElementTrait for BPMNExpandedSubProcess {
    fn add_incoming_sequence_flow(&mut self, flow_index: usize) -> anyhow::Result<()> {
        self.incoming_sequence_flows.push(flow_index);
        Ok(())
    }

    fn add_outgoing_sequence_flow(&mut self, flow_index: usize) -> anyhow::Result<()> {
        self.outgoing_sequence_flows.push(flow_index);
        Ok(())
    }

    fn add_incoming_message_flow(&mut self, _flow_index: usize) -> Result<()> {
        Err(anyhow!(
            "expanded sub-processes cannot have incoming message flows"
        ))
    }

    fn add_outgoing_message_flow(&mut self, _flow_index: usize) -> Result<()> {
        Err(anyhow!(
            "expanded sub-processes cannot have outgoing message flows"
        ))
    }

    fn verify_structural_correctness(&self, bpmn: &BusinessProcessModelAndNotation) -> Result<()> {
        for element in &self.elements {
            element.verify_structural_correctness(bpmn)?
        }
        Ok(())
    }
}

impl BPMNObject for BPMNExpandedSubProcess {
    fn index(&self) -> usize {
        self.index
    }

    fn id(&self) -> &str {
        &self.id
    }

    fn incoming_sequence_flows(&self) -> &[usize] {
        &self.incoming_sequence_flows
    }

    fn outgoing_sequence_flows(&self) -> &[usize] {
        &self.outgoing_sequence_flows
    }

    fn incoming_message_flows(&self) -> &[usize] {
        &EMPTY_FLOWS
    }

    fn outgoing_message_flows(&self) -> &[usize] {
        &EMPTY_FLOWS
    }

    fn can_have_incoming_sequence_flows(&self) -> bool {
        true
    }
}

impl Transitionable for BPMNExpandedSubProcess {
    fn number_of_transitions(&self) -> usize {
        //one transition to start, one transition to finish
        2
    }
}
