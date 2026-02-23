use crate::bpmn::element::BPMNElement;

pub trait Elementable {
    fn all_elements_ref(&self) -> Vec<&BPMNElement>;

    /// find an element with the given index
    fn index_2_element(&self, index: usize) -> Option<&BPMNElement>;

    /// find an element with the given index
    fn index_2_element_mut(&mut self, index: usize) -> Option<&mut BPMNElement>;
}

impl Elementable for Vec<BPMNElement> {
    fn all_elements_ref(&self) -> Vec<&BPMNElement> {
        self.iter()
            .map(|element| element.all_elements_ref())
            .flatten()
            .collect()
    }

    fn index_2_element(&self, index: usize) -> Option<&BPMNElement> {
        self.iter()
            .filter_map(|element| element.index_2_element(index))
            .next()
    }

    fn index_2_element_mut(&mut self, index: usize) -> Option<&mut BPMNElement> {
        self.iter_mut()
            .filter_map(|element| element.index_2_element_mut(index))
            .next()
    }
}
