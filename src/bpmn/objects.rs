use crate::bpmn::{collapsed_pool::BPMNCollapsedPool, element::BPMNElement, process::BPMNProcess};

pub trait IdSearchable {
    /// find an object with the given index
    fn index_2_object(&self, index: usize) -> Option<&dyn BPMNObject>;

    /// find an object with the given id, returns (pool index, element index)
    fn id_2_pool_and_index(&self, id: &str) -> Option<(Option<usize>, usize)>;

    fn all_elements_ref(&self) -> Vec<&BPMNElement>;

    /// find an element with the given indexs
    fn index_2_element_mut(&mut self, index: usize) -> Option<&mut BPMNElement>;
}

pub trait BPMNObject: IdSearchable {
    fn index(&self) -> usize;

    fn id(&self) -> &str;

    /// the flow indices of the outgoing sequence flows of this object
    fn outgoing_sequence_flows(&self) -> &Vec<usize>;

    fn can_have_incoming_sequence_flow(&self) -> bool;

    fn can_catch_message(&self) -> bool;

    fn can_throw_message(&self) -> bool;
}

macro_rules! of_vec {
    ($t:ident) => {
        impl IdSearchable for Vec<$t> {
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

            fn all_elements_ref(&self) -> Vec<&BPMNElement> {
                self.iter()
                    .map(|element| element.all_elements_ref())
                    .flatten()
                    .collect()
            }

            fn index_2_element_mut(&mut self, index: usize) -> Option<&mut BPMNElement> {
                self.iter_mut()
                    .filter_map(|element| element.index_2_element_mut(index))
                    .next()
            }
        }
    };
}

of_vec!(BPMNElement);
of_vec!(BPMNCollapsedPool);

impl IdSearchable for Vec<BPMNProcess> {
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
            if let Some((_, x)) = element.id_2_pool_and_index(id) {
                return Some((Some(element.index), x));
            }
        }
        None
    }

    fn all_elements_ref(&self) -> Vec<&BPMNElement> {
        self.iter()
            .map(|element| element.all_elements_ref())
            .flatten()
            .collect()
    }

    fn index_2_element_mut(&mut self, index: usize) -> Option<&mut BPMNElement> {
        self.iter_mut()
            .filter_map(|element| element.index_2_element_mut(index))
            .next()
    }
}

impl IdSearchable for (&mut Vec<BPMNProcess>, &mut Vec<BPMNCollapsedPool>) {
    fn index_2_object(&self, index: usize) -> Option<&dyn BPMNObject> {
        let x = self.0.index_2_object(index);
        if x.is_some() {
            return x;
        }
        self.1.index_2_object(index)
    }

    fn id_2_pool_and_index(&self, id: &str) -> Option<(Option<usize>, usize)> {
        let x = self.0.id_2_pool_and_index(id);
        if x.is_some() {
            return x;
        }
        self.1.id_2_pool_and_index(id)
    }

    fn all_elements_ref(&self) -> Vec<&BPMNElement> {
        let mut x = self.0.all_elements_ref();
        x.extend(self.1.all_elements_ref());
        x
    }

    fn index_2_element_mut(&mut self, index: usize) -> Option<&mut BPMNElement> {
        let x = self.0.index_2_element_mut(index);
        if x.is_some() {
            return x;
        }
        self.1.index_2_element_mut(index)
    }
}
