use crate::bpmn::{
    element::BPMNElementTrait,
    objects_objectable::{BPMNObject, EMPTY_FLOWS},
    objects_transitionable::Transitionable,
};
use anyhow::{Result, anyhow};

#[derive(Clone, Debug)]
pub struct BPMNCollapsedPool {
    pub index: usize,
    pub id: String,
    pub name: Option<String>,
    pub incoming_message_flows: Vec<usize>,
    pub outgoing_message_flows: Vec<usize>,
}

impl BPMNElementTrait for BPMNCollapsedPool {
    fn verify_structural_correctness(
        &self,
        _bpmn: &crate::BusinessProcessModelAndNotation,
    ) -> Result<()> {
        Ok(())
    }

    fn add_incoming_sequence_flow(&mut self, _flow_index: usize) -> Result<()> {
        return Err(anyhow!("cannot add sequence flow to collapsed pool"));
    }

    fn add_outgoing_sequence_flow(&mut self, _flow_index: usize) -> Result<()> {
        return Err(anyhow!("cannot add sequence flow from collapsed pool"));
    }

    fn add_incoming_message_flow(&mut self, flow_index: usize) -> Result<()> {
        self.incoming_message_flows.push(flow_index);
        Ok(())
    }

    fn add_outgoing_message_flow(&mut self, flow_index: usize) -> Result<()> {
        self.outgoing_message_flows.push(flow_index);
        Ok(())
    }
}

impl BPMNObject for BPMNCollapsedPool {
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
        &EMPTY_FLOWS
    }

    fn incoming_message_flows(&self) -> &[usize] {
        &self.incoming_message_flows
    }

    fn outgoing_message_flows(&self) -> &[usize] {
        &self.outgoing_message_flows
    }
    fn can_have_incoming_sequence_flows(&self) -> bool {
        false
    }
}

impl Transitionable for BPMNCollapsedPool {
    fn number_of_transitions(&self) -> usize {
        0
    }
}
