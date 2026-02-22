use crate::bpmn::{
    element::BPMNElement,
    objects::{BPMNObject, IdSearchable},
};

static EMPTY_SEQUENCE_FLOWS: Vec<usize> = vec![];

#[derive(Clone, Debug)]
pub struct SequenceFlow {
    pub index: usize,
    pub id: String,
    pub flow_index: usize,
    pub source_index: usize,
    pub target_index: usize,
}

impl IdSearchable for SequenceFlow {
    fn index_2_object(&self, index: usize) -> Option<&dyn BPMNObject> {
        if self.index == index {
            Some(self)
        } else {
            None
        }
    }

    fn id_2_pool_and_index(&self, id: &str) -> Option<(Option<usize>, usize)> {
        if self.id == id {
            Some((None, self.index))
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

impl BPMNObject for SequenceFlow {
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
