use crate::bpmn::{element::BPMNElement, objects_objectable::BPMNObject};

pub trait Searchable {
    /// find an object with the given index
    fn index_2_object(&self, index: usize) -> Option<&dyn BPMNObject>;

    /// find an object with the given id, returns (pool index, element index)
    fn id_2_pool_and_index(&self, id: &str) -> Option<(Option<usize>, usize)>;
}

impl Searchable for Vec<BPMNElement> {
    fn index_2_object(&self, index: usize) -> Option<&dyn BPMNObject> {
        for process in self {
            let x = process.index_2_object(index);
            if x.is_some() {
                return x;
            }
        }
        None
    }

    fn id_2_pool_and_index(&self, id: &str) -> Option<(Option<usize>, usize)> {
        for element in self {
            let x = element.id_2_pool_and_index(id);
            if x.is_some() {
                return x;
            }
        }
        None
    }
}
