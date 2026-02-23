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
pub struct BPMNMessageEndEvent {
    pub(crate) index: usize,
    pub(crate) id: String,
    pub(crate) message_marker_index: usize,
    pub(crate) message_marker_id: String,
    pub(crate) incoming_sequence_flows: Vec<usize>,
    pub(crate) outgoing_message_flow: Option<usize>,
}

impl BPMNElementTrait for BPMNMessageEndEvent {
    fn add_incoming_sequence_flow(&mut self, flow_index: usize) -> Result<()> {
        self.incoming_sequence_flows.push(flow_index);
        Ok(())
    }

    fn add_outgoing_sequence_flow(&mut self, _flow_index: usize) -> Result<()> {
        Err(anyhow!(
            "message end events cannot have outgoing sequence flows"
        ))
    }

    fn add_incoming_message_flow(&mut self, _flow_index: usize) -> Result<()> {
        Err(anyhow!(
            "message end events cannot have incoming message flows"
        ))
    }

    fn add_outgoing_message_flow(&mut self, flow_index: usize) -> Result<()> {
        if self.outgoing_message_flow.is_some() {
            return Err(anyhow!("cannot add a second outgoing message flow"));
        }
        self.outgoing_message_flow = Some(flow_index);
        Ok(())
    }

    fn verify_structural_correctness(&self, _bpmn: &BusinessProcessModelAndNotation) -> Result<()> {
        Ok(())
    }
}

impl BPMNObject for BPMNMessageEndEvent {
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
        &EMPTY_FLOWS
    }

    fn incoming_message_flows(&self) -> &[usize] {
        &EMPTY_FLOWS
    }

    fn outgoing_message_flows(&self) -> &[usize] {
        self.outgoing_message_flow.as_slice()
    }

    fn can_have_incoming_sequence_flows(&self) -> bool {
        true
    }
}

impl Transitionable for BPMNMessageEndEvent {
    fn number_of_transitions(&self) -> usize {
        self.incoming_sequence_flows.len()
    }
}
