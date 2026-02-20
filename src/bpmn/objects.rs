use crate::Activity;

#[derive(Clone, Debug)]
pub struct BPMNProcess {
    pub(crate) index: usize,
    pub(crate) id: String,
    pub elements: Vec<BPMNElement>,
    pub sequence_flows: Vec<SequenceFlow>,
}

#[derive(Clone, Debug)]
pub enum BPMNElement {
    EndEvent {
        index: usize,
        id: String,
    },

    ExclusiveGateway {
        index: usize,
        id: String,
    },
    InclusiveGateway {
        index: usize,
        id: String,
    },
    IntermediateCatchEvent {
        index: usize,
        id: String,
    },
    IntermediateThrowEvent {
        index: usize,
        id: String,
    },
    MessageEndEvent {
        index: usize,
        id: String,
        message_index: usize,
        message_id: String,
    },
    MessageIntermediateCatchEvent {
        index: usize,
        id: String,
        message_index: usize,
        message_id: String,
    },
    MessageIntermediateThrowEvent {
        index: usize,
        id: String,
        message_index: usize,
        message_id: String,
    },
    MessageStartEvent {
        index: usize,
        id: String,
        message_index: usize,
        message_id: String,
    },
    StartEvent {
        index: usize,
        id: String,
    },
    Task {
        index: usize,
        id: String,
        activity: Activity,
    },
}

#[derive(Clone, Debug)]
pub struct SequenceFlow {
    pub index: usize,
    pub id: String,
    pub source_element_index: usize,
    pub target_element_index: usize,
}

#[derive(Clone, Debug)]
pub struct MessageFlow {
    pub index: usize,
    pub id: String,
    pub source_process_rank: usize,
    pub source_element_index: usize,
    pub target_process_rank: usize,
    pub target_element_index: usize,
}

pub trait IdSearchable {
    /// find an object with the given id, returns (pool rank, element index)
    fn search_id(&self, id: &str) -> Option<(Option<usize>, usize)>;
}

pub trait BPMNObject: IdSearchable {
    /// find an object with the given id
    fn find_object_with_index(&self, index: usize) -> Option<&dyn BPMNObject>;

    fn id(&self) -> &str;

    fn can_catch_message(&self) -> bool;

    fn can_throw_message(&self) -> bool;
}

impl IdSearchable for MessageFlow {
    fn search_id(&self, id: &str) -> Option<(Option<usize>, usize)> {
        if self.id == id {
            Some((None, self.index))
        } else {
            None
        }
    }
}

impl BPMNObject for MessageFlow {
    fn find_object_with_index(&self, index: usize) -> Option<&dyn BPMNObject> {
        if self.index == index {
            Some(self)
        } else {
            None
        }
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
}

impl IdSearchable for BPMNElement {
    fn search_id(&self, search_id: &str) -> Option<(Option<usize>, usize)> {
        match self {
            BPMNElement::StartEvent { index, id }
            | BPMNElement::IntermediateThrowEvent { index, id }
            | BPMNElement::IntermediateCatchEvent { index, id }
            | BPMNElement::EndEvent { index, id }
            | BPMNElement::Task { index, id, .. }
            | BPMNElement::ExclusiveGateway { index, id }
            | BPMNElement::InclusiveGateway { index, id } => {
                if search_id == id {
                    Some((None, *index))
                } else {
                    None
                }
            }
            BPMNElement::MessageStartEvent {
                index,
                id,
                message_index,
                message_id,
            }
            | BPMNElement::MessageEndEvent {
                index,
                id,
                message_index,
                message_id,
            }
            | BPMNElement::MessageIntermediateThrowEvent {
                index,
                id,
                message_index,
                message_id,
            }
            | BPMNElement::MessageIntermediateCatchEvent {
                index,
                id,
                message_index,
                message_id,
            } => {
                if search_id == id {
                    Some((None, *index))
                } else if search_id == message_id {
                    Some((None, *message_index))
                } else {
                    None
                }
            }
        }
    }
}

impl BPMNObject for BPMNElement {
    fn find_object_with_index(&self, search_index: usize) -> Option<&dyn BPMNObject> {
        match self {
            BPMNElement::StartEvent { index, .. }
            | BPMNElement::IntermediateThrowEvent { index, .. }
            | BPMNElement::IntermediateCatchEvent { index, .. }
            | BPMNElement::EndEvent { index, .. }
            | BPMNElement::Task { index, .. }
            | BPMNElement::ExclusiveGateway { index, .. }
            | BPMNElement::MessageStartEvent { index, .. }
            | BPMNElement::MessageEndEvent { index, .. }
            | BPMNElement::MessageIntermediateCatchEvent { index, .. }
            | BPMNElement::MessageIntermediateThrowEvent { index, .. }
            | BPMNElement::InclusiveGateway { index, .. } => {
                if search_index == *index {
                    Some(self)
                } else {
                    None
                }
            }
        }
    }

    fn id(&self) -> &str {
        match self {
            BPMNElement::StartEvent { id, .. }
            | BPMNElement::IntermediateThrowEvent { id, .. }
            | BPMNElement::IntermediateCatchEvent { id, .. }
            | BPMNElement::EndEvent { id, .. }
            | BPMNElement::Task { id, .. }
            | BPMNElement::ExclusiveGateway { id, .. }
            | BPMNElement::MessageStartEvent { id, .. }
            | BPMNElement::MessageEndEvent { id, .. }
            | BPMNElement::MessageIntermediateCatchEvent { id, .. }
            | BPMNElement::MessageIntermediateThrowEvent { id, .. }
            | BPMNElement::InclusiveGateway { id, .. } => id,
        }
    }

    fn can_catch_message(&self) -> bool {
        match self {
            BPMNElement::MessageIntermediateCatchEvent { .. }
            | BPMNElement::MessageStartEvent { .. }
            | BPMNElement::Task { .. } => true,
            _ => false,
        }
    }

    fn can_throw_message(&self) -> bool {
        match self {
            BPMNElement::MessageIntermediateThrowEvent { .. }
            | BPMNElement::MessageEndEvent { .. }
            | BPMNElement::Task { .. } => true,
            _ => false,
        }
    }
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

    fn id(&self) -> &str {
        &self.id
    }

    fn can_catch_message(&self) -> bool {
        false
    }

    fn can_throw_message(&self) -> bool {
        false
    }
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

    fn id(&self) -> &str {
        &self.id
    }

    fn can_catch_message(&self) -> bool {
        false
    }

    fn can_throw_message(&self) -> bool {
        false
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
