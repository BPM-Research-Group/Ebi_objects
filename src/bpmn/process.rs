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
}

impl BPMNObject for BPMNProcess {
    fn find_object_with_index(&self, index: usize) -> Option<&dyn BPMNObject> {
        if self.index == index {
            return Some(self);
        } else {
            for element in &self.elements {
                let x = element.find_object_with_index(index);
                if x.is_some() {
                    return x;
                }
            }
        }
        None
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
        self.elements
            .iter()
            .map(|element| element.all_elements_ref())
            .flatten()
            .collect()
    }

    fn element_mut(&mut self, index: usize) -> Option<&mut BPMNElement> {
        self.elements
            .iter_mut()
            .filter_map(|element| element.element_mut(index))
            .next()
    }
}
