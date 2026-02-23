use crate::{
    Activity, BusinessProcessModelAndNotation,
    bpmn::{
        element::BPMNElementTrait, objects_objectable::BPMNObject,
        objects_transitionable::Transitionable,
    },
};
use anyhow::{Result, anyhow};

#[derive(Debug, Clone)]
pub struct BPMNTask {
    pub(crate) index: usize,
    pub(crate) id: String,
    pub activity: Activity,
    pub(crate) incoming_sequence_flows: Vec<usize>,
    pub(crate) outgoing_sequence_flows: Vec<usize>,
    pub(crate) incoming_message_flow: Option<usize>,
    pub(crate) outgoing_message_flow: Option<usize>,
}

impl BPMNElementTrait for BPMNTask {
    fn add_incoming_sequence_flow(&mut self, flow_index: usize) -> Result<()> {
        self.incoming_sequence_flows.push(flow_index);
        Ok(())
    }

    fn add_outgoing_sequence_flow(&mut self, flow_index: usize) -> Result<()> {
        self.outgoing_sequence_flows.push(flow_index);
        Ok(())
    }

    fn add_incoming_message_flow(&mut self, flow_index: usize) -> Result<()> {
        if self.incoming_message_flow.is_some() {
            return Err(anyhow!("cannot add a second incoming message flow"));
        }
        self.incoming_message_flow = Some(flow_index);
        Ok(())
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

impl BPMNObject for BPMNTask {
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

    fn can_have_incoming_sequence_flows(&self) -> bool {
        true
    }

    fn incoming_message_flows(&self) -> &[usize] {
        &self.incoming_message_flow.as_slice()
    }

    fn outgoing_message_flows(&self) -> &[usize] {
        &self.outgoing_message_flow.as_slice()
    }
}

impl Transitionable for BPMNTask {
    fn number_of_transitions(&self) -> usize {
        self.incoming_sequence_flows.len()
    }
}
