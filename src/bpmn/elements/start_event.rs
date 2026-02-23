use crate::{
    BusinessProcessModelAndNotation,
    bpmn::{
        element::BPMNElementTrait,
        objects_objectable::{BPMNObject, EMPTY_FLOWS},
        objects_transitionable::Transitionable,
    },
};
use anyhow::{Result, anyhow};

#[derive(Debug, Clone)]
pub struct BPMNStartEvent {
    pub(crate) index: usize,
    pub(crate) id: String,
    pub(crate) outgoing_sequence_flows: Vec<usize>,
}

impl BPMNElementTrait for BPMNStartEvent {
    fn add_incoming_sequence_flow(&mut self, _flow_index: usize) -> Result<()> {
        Err(anyhow!(
            "message start events cannot have incoming sequence flows"
        ))
    }

    fn add_outgoing_sequence_flow(&mut self, flow_index: usize) -> Result<()> {
        self.outgoing_sequence_flows.push(flow_index);
        Ok(())
    }

    fn add_incoming_message_flow(&mut self, _flow_index: usize) -> Result<()> {
        Err(anyhow!(
            "message start events cannot have incoming message flows"
        ))
    }

    fn add_outgoing_message_flow(&mut self, _flow_index: usize) -> Result<()> {
        Err(anyhow!(
            "message start events cannot have outgoing message flows"
        ))
    }

    fn verify_structural_correctness(&self, _bpmn: &BusinessProcessModelAndNotation) -> Result<()> {
        Ok(())
    }
}

impl BPMNObject for BPMNStartEvent {
    fn index(&self) -> usize {
        self.index
    }

    fn id(&self) -> &str {
        &self.id
    }

    fn incoming_sequence_flows(&self) -> &[usize] {
        &EMPTY_FLOWS
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
        false
    }
}

impl Transitionable for BPMNStartEvent {
    fn number_of_transitions(&self) -> usize {
        0
    }
}
