use crate::bpmn::{
    element::BPMNElement,
    objects::{BPMNObject, IdSearchable},
};

#[derive(Clone, Debug)]
pub struct BPMNCollapsedPool {
    pub index: usize,
    pub id: String,
    pub name: Option<String>,
    pub outgoing_sequence_flows: Vec<usize>,
}

impl IdSearchable for BPMNCollapsedPool {
    fn index_2_object(&self, index: usize) -> Option<&dyn BPMNObject> {
        if self.index == index {
            Some(self)
        } else {
            None
        }
    }

    fn id_2_pool_and_index(&self, id: &str) -> Option<(Option<usize>, usize)> {
        if self.id == id {
            Some((Some(self.index), self.index))
        } else {
            None
        }
    }

    fn all_elements_ref(&self) -> Vec<&BPMNElement> {
        vec![]
    }

    fn index_2_element_mut(&mut self, _index: usize) -> Option<&mut BPMNElement> {
        None
    }
}

impl BPMNObject for BPMNCollapsedPool {
    fn index(&self) -> usize {
        self.index
    }

    fn id(&self) -> &str {
        &self.id
    }

    fn outgoing_sequence_flows(&self) -> &Vec<usize> {
        &self.outgoing_sequence_flows
    }

    fn can_have_incoming_sequence_flow(&self) -> bool {
        false
    }

    fn can_catch_message(&self) -> bool {
        true
    }

    fn can_throw_message(&self) -> bool {
        true
    }
}
