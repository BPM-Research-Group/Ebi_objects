use crate::bpmn::{
    element::BPMNElement,
    objects::{BPMNObject, IdSearchable},
};

static EMPTY_SEQUENCE_FLOWS: Vec<usize> = vec![];

#[derive(Clone, Debug)]
pub struct BPMNProcess {
    pub(crate) index: usize,
    pub(crate) id: String,
    pub elements: Vec<BPMNElement>,
}

impl IdSearchable for BPMNProcess {
    fn index_2_object(&self, index: usize) -> Option<&dyn BPMNObject> {
        if self.index == index {
            return Some(self);
        }
        self.elements.index_2_object(index)
    }

    fn id_2_pool_and_index(&self, id: &str) -> Option<(Option<usize>, usize)> {
        if self.id == id {
            Some((None, self.index))
        } else {
            if let Some((_, index)) = self.elements.id_2_pool_and_index(id) {
                Some((None, index))
            } else {
                None
            }
        }
    }

    fn all_elements_ref(&self) -> Vec<&BPMNElement> {
        self.elements.all_elements_ref()
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

    fn outgoing_sequence_flows(&self) -> &Vec<usize> {
        &EMPTY_SEQUENCE_FLOWS
    }

    fn can_have_incoming_sequence_flow(&self) -> bool {
        false
    }

    fn can_catch_message(&self) -> bool {
        false
    }

    fn can_throw_message(&self) -> bool {
        false
    }
}
