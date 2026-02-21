use crate::bpmn::{
    element::BPMNElement,
    objects::{BPMNObject, IdSearchable},
    sequence_flow::SequenceFlow,
};

#[derive(Clone, Debug)]
pub struct BPMNProcess {
    pub(crate) index: usize,
    pub(crate) id: String,
    pub elements: Vec<BPMNElement>,
    pub sequence_flows: Vec<SequenceFlow>,
}

impl IdSearchable for BPMNProcess {
    fn find_object_with_index(&self, index: usize) -> Option<&dyn BPMNObject> {
        if self.index == index {
            return Some(self);
        }
        self.elements.find_object_with_index(index)
    }

    fn search_id(&self, id: &str) -> Option<(Option<usize>, usize)> {
        if self.id == id {
            Some((None, self.index))
        } else {
            if let Some((_, index)) = self.elements.search_id(id) {
                Some((None, index))
            } else {
                None
            }
        }
    }

    fn number_of_flows(&self) -> usize {
        self.sequence_flows.len() + self.elements.number_of_flows()
    }

    fn all_elements_ref(&self) -> Vec<&BPMNElement> {
        self.elements.all_elements_ref()
    }

    fn element_mut(&mut self, index: usize) -> Option<&mut BPMNElement> {
        self.elements.element_mut(index)
    }
}

impl BPMNObject for BPMNProcess {
    fn index(&self) -> usize {
        self.index
    }

    fn id(&self) -> &str {
        &self.id
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
