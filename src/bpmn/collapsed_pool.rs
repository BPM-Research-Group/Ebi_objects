use crate::bpmn::{
    element::BPMNElement,
    objects::{BPMNObject, IdSearchable},
};

#[derive(Clone, Debug)]
pub struct BPMNCollapsedPool {
    pub index: usize,
    pub id: String,
    pub name: Option<String>,
}

impl IdSearchable for BPMNCollapsedPool {
    fn find_object_with_index(&self, index: usize) -> Option<&dyn BPMNObject> {
        if self.index == index {
            Some(self)
        } else {
            None
        }
    }

    fn search_id(&self, id: &str) -> Option<(Option<usize>, usize)> {
        if self.id == id {
            Some((Some(self.index), self.index))
        } else {
            None
        }
    }

    fn number_of_flows(&self) -> usize {
        0
    }

    fn all_elements_ref(&self) -> Vec<&BPMNElement> {
        vec![]
    }

    fn element_mut(&mut self, _index: usize) -> Option<&mut BPMNElement> {
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
