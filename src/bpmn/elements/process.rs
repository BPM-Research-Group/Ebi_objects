use crate::{
    BusinessProcessModelAndNotation,
    bpmn::{
        element::{BPMNElement, BPMNElementTrait},
        objects_elementable::Elementable,
        objects_objectable::{BPMNObject, EMPTY_FLOWS},
        objects_searchable::Searchable,
        objects_transitionable::Transitionable,
    },
};
use anyhow::{Result, anyhow};

#[derive(Clone, Debug)]
pub struct BPMNProcess {
    pub index: usize,
    pub id: String,
    pub elements: Vec<BPMNElement>,
}

impl Searchable for BPMNProcess {
    fn index_2_object(&self, index: usize) -> Option<&dyn BPMNObject> {
        if self.index == index {
            return Some(self);
        }
        self.elements.index_2_object(index)
    }

    fn id_2_pool_and_index(&self, id: &str) -> Option<(Option<usize>, usize)> {
        if self.id == id {
            Some((Some(self.index), self.index))
        } else {
            if let Some((_, index)) = self.elements.id_2_pool_and_index(id) {
                Some((Some(self.index), index))
            } else {
                None
            }
        }
    }
}

impl BPMNElementTrait for BPMNProcess {
    fn verify_structural_correctness(&self, bpmn: &BusinessProcessModelAndNotation) -> Result<()> {
        for element in &self.elements {
            element.verify_structural_correctness(bpmn)?
        }
        Ok(())
    }

    fn add_incoming_sequence_flow(&mut self, _flow_index: usize) -> anyhow::Result<()> {
        Err(anyhow!("processes cannot have incoming sequence flows"))
    }

    fn add_outgoing_sequence_flow(&mut self, _flow_index: usize) -> anyhow::Result<()> {
        Err(anyhow!("processes cannot have outgoing sequence flows"))
    }

    fn add_incoming_message_flow(&mut self, _flow_index: usize) -> anyhow::Result<()> {
        Err(anyhow!("processes cannot have incoming message flows"))
    }

    fn add_outgoing_message_flow(&mut self, _flow_index: usize) -> anyhow::Result<()> {
        Err(anyhow!("processes cannot have outgoing message flows"))
    }
}

impl Elementable for BPMNProcess {
    fn all_elements_ref(&self) -> Vec<&BPMNElement> {
        self.elements.all_elements_ref()
    }

    fn index_2_element(&self, index: usize) -> Option<&BPMNElement> {
        self.elements.index_2_element(index)
    }

    fn index_2_element_mut(&mut self, index: usize) -> Option<&mut BPMNElement> {
        self.elements.index_2_element_mut(index)
    }
}

impl BPMNObject for BPMNProcess {
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
        &EMPTY_FLOWS
    }

    fn outgoing_message_flows(&self) -> &[usize] {
        &EMPTY_FLOWS
    }

    fn can_have_incoming_sequence_flows(&self) -> bool {
        false
    }
}

impl Transitionable for BPMNProcess {
    fn number_of_transitions(&self) -> usize {
        self.elements
            .iter()
            .map(|element| element.number_of_transitions())
            .sum()
    }
}
