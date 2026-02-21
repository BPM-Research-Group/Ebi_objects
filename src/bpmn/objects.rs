use crate::bpmn::{collapsed_pool::BPMNCollapsedPool, element::BPMNElement, process::BPMNProcess};

pub trait IdSearchable {
    /// find an object with the given id
    fn find_object_with_index(&self, index: usize) -> Option<&dyn BPMNObject>;

    /// find an object with the given id, returns (pool index, element index)
    fn search_id(&self, id: &str) -> Option<(Option<usize>, usize)>;

    fn number_of_flows(&self) -> usize;

    fn all_elements_ref(&self) -> Vec<&BPMNElement>;

    fn element_mut(&mut self, index: usize) -> Option<&mut BPMNElement>;
}

pub trait BPMNObject: IdSearchable {
    fn index(&self) -> usize;

    fn id(&self) -> &str;

    fn can_have_incoming_sequence_flow(&self) -> bool;

    fn can_catch_message(&self) -> bool;

    fn can_throw_message(&self) -> bool;
}

macro_rules! of_vec {
    ($t:ident) => {
        impl IdSearchable for Vec<$t> {
            fn find_object_with_index(&self, index: usize) -> Option<&dyn BPMNObject> {
                for process in self {
                    let x = process.find_object_with_index(index);
                    if x.is_some() {
                        return x;
                    }
                }
                None
            }

            fn search_id(&self, id: &str) -> Option<(Option<usize>, usize)> {
                for element in self {
                    let x = element.search_id(id);
                    if x.is_some() {
                        return x;
                    }
                }
                None
            }

            fn number_of_flows(&self) -> usize {
                self.iter().map(|element| element.number_of_flows()).sum()
            }

            fn all_elements_ref(&self) -> Vec<&BPMNElement> {
                self.iter()
                    .map(|element| element.all_elements_ref())
                    .flatten()
                    .collect()
            }

            fn element_mut(&mut self, index: usize) -> Option<&mut BPMNElement> {
                self.iter_mut()
                    .filter_map(|element| element.element_mut(index))
                    .next()
            }
        }
    };
}

of_vec!(BPMNElement);
of_vec!(BPMNCollapsedPool);

impl IdSearchable for Vec<BPMNProcess> {
    fn find_object_with_index(&self, index: usize) -> Option<&dyn BPMNObject> {
        for process in self {
            let x = process.find_object_with_index(index);
            if x.is_some() {
                return x;
            }
        }
        None
    }

    fn search_id(&self, id: &str) -> Option<(Option<usize>, usize)> {
        for element in self {
            if let Some((_, x)) = element.search_id(id) {
                return Some((Some(element.index), x));
            }
        }
        None
    }

    fn number_of_flows(&self) -> usize {
        self.iter().map(|element| element.number_of_flows()).sum()
    }

    fn all_elements_ref(&self) -> Vec<&BPMNElement> {
        self.iter()
            .map(|element| element.all_elements_ref())
            .flatten()
            .collect()
    }

    fn element_mut(&mut self, index: usize) -> Option<&mut BPMNElement> {
        self.iter_mut()
            .filter_map(|element| element.element_mut(index))
            .next()
    }
}

impl IdSearchable for (&mut Vec<BPMNProcess>, &mut Vec<BPMNCollapsedPool>) {
    fn find_object_with_index(&self, index: usize) -> Option<&dyn BPMNObject> {
        let x = self.0.find_object_with_index(index);
        if x.is_some() {
            return x;
        }
        self.1.find_object_with_index(index)
    }

    fn search_id(&self, id: &str) -> Option<(Option<usize>, usize)> {
        let x = self.0.search_id(id);
        if x.is_some() {
            return x;
        }
        self.1.search_id(id)
    }

    fn number_of_flows(&self) -> usize {
        self.0.number_of_flows() + self.1.number_of_flows()
    }

    fn all_elements_ref(&self) -> Vec<&BPMNElement> {
        let mut x = self.0.all_elements_ref();
        x.extend(self.1.all_elements_ref());
        x
    }

    fn element_mut(&mut self, index: usize) -> Option<&mut BPMNElement> {
        let x = self.0.element_mut(index);
        if x.is_some() {
            return x;
        }
        self.1.element_mut(index)
    }
}
