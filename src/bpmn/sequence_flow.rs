use crate::bpmn::{
    element::BPMNElement,
    objects::{BPMNObject, IdSearchable},
};

#[derive(Clone, Debug)]
pub struct SequenceFlow {
    pub index: usize,
    pub id: String,
    pub source_element_index: usize,
    pub target_element_index: usize,
}

impl IdSearchable for SequenceFlow {
    fn search_id(&self, id: &str) -> Option<(Option<usize>, usize)> {
        if self.id == id {
            Some((None, self.index))
        } else {
            None
        }
    }
}

impl BPMNObject for SequenceFlow {
    fn find_object_with_index(&self, index: usize) -> Option<&dyn BPMNObject> {
        if self.index == index {
            Some(self)
        } else {
            None
        }
    }

    fn index(&self) -> usize {
        self.index
    }

    fn id(&self) -> &str {
        &self.id
    }

    fn can_catch_message(&self) -> bool {
        false
    }

    fn can_throw_message(&self) -> bool {
        false
    }

    fn all_elements_ref(&self) -> Vec<&BPMNElement> {
        vec![]
    }

    fn element_mut(&mut self, _index: usize) -> Option<&mut BPMNElement> {
        None
    }
}
