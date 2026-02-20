use crate::bpmn::{element::BPMNElement, process::BPMNProcess};

pub trait IdSearchable {
    /// find an object with the given id, returns (pool rank, element index)
    fn search_id(&self, id: &str) -> Option<(Option<usize>, usize)>;
}

pub trait BPMNObject: IdSearchable {
    /// find an object with the given id
    fn find_object_with_index(&self, index: usize) -> Option<&dyn BPMNObject>;

    fn index(&self) -> usize;

    fn id(&self) -> &str;

    fn can_catch_message(&self) -> bool;

    fn can_throw_message(&self) -> bool;

    fn all_elements_ref(&self) -> Vec<&BPMNElement>;

    fn element_mut(&mut self, index: usize) -> Option<&mut BPMNElement>;
}

impl IdSearchable for Vec<BPMNElement> {
    fn search_id(&self, id: &str) -> Option<(Option<usize>, usize)> {
        for element in self {
            let x = element.search_id(id);
            if x.is_some() {
                return x;
            }
        }
        None
    }
}

impl IdSearchable for Vec<BPMNProcess> {
    fn search_id(&self, id: &str) -> Option<(Option<usize>, usize)> {
        for (process_rank, process) in self.iter().enumerate() {
            if let Some((_, element_index)) = process.search_id(id) {
                return Some((Some(process_rank), element_index));
            }
        }
        None
    }
}
