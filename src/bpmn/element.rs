use crate::{
    Activity,
    bpmn::{
        objects::{BPMNObject, IdSearchable},
        sequence_flow::SequenceFlow,
    },
};
use strum_macros::EnumIs;

#[derive(Clone, Debug, EnumIs)]
pub enum BPMNElement {
    EndEvent {
        index: usize,
        id: String,
    },
    EventBasedGateway {
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
    ParallelGateway {
        index: usize,
        id: String,
    },
    StartEvent {
        index: usize,
        id: String,
    },
    ExpandedSubProcess {
        index: usize,
        id: String,
        name: Option<String>,
        elements: Vec<BPMNElement>,
        sequence_flows: Vec<SequenceFlow>,
    },
    CollapsedSubProcess {
        index: usize,
        id: String,
        name: Option<String>,
    },
    Task {
        index: usize,
        id: String,
        activity: Activity,
    },
}

impl IdSearchable for BPMNElement {
    fn find_object_with_index(&self, search_index: usize) -> Option<&dyn BPMNObject> {
        match self {
            BPMNElement::StartEvent { index, .. }
            | BPMNElement::IntermediateThrowEvent { index, .. }
            | BPMNElement::IntermediateCatchEvent { index, .. }
            | BPMNElement::EndEvent { index, .. }
            | BPMNElement::Task { index, .. }
            | BPMNElement::CollapsedSubProcess { index, .. }
            | BPMNElement::ExclusiveGateway { index, .. }
            | BPMNElement::MessageStartEvent { index, .. }
            | BPMNElement::MessageEndEvent { index, .. }
            | BPMNElement::MessageIntermediateCatchEvent { index, .. }
            | BPMNElement::MessageIntermediateThrowEvent { index, .. }
            | BPMNElement::ParallelGateway { index, .. }
            | BPMNElement::EventBasedGateway { index, .. }
            | BPMNElement::InclusiveGateway { index, .. } => {
                if search_index == *index {
                    Some(self)
                } else {
                    None
                }
            }
            BPMNElement::ExpandedSubProcess {
                index, elements, ..
            } => {
                if search_index == *index {
                    Some(self)
                } else {
                    elements.find_object_with_index(search_index)
                }
            }
        }
    }

    fn search_id(&self, search_id: &str) -> Option<(Option<usize>, usize)> {
        match self {
            BPMNElement::StartEvent { index, id }
            | BPMNElement::IntermediateThrowEvent { index, id }
            | BPMNElement::IntermediateCatchEvent { index, id }
            | BPMNElement::EndEvent { index, id }
            | BPMNElement::CollapsedSubProcess { index, id, .. }
            | BPMNElement::ExclusiveGateway { index, id }
            | BPMNElement::InclusiveGateway { index, id }
            | BPMNElement::EventBasedGateway { index, id }
            | BPMNElement::Task { index, id, .. }
            | BPMNElement::ParallelGateway { index, id } => {
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
            BPMNElement::ExpandedSubProcess {
                index,
                id,
                elements,
                ..
            } => {
                if search_id == id {
                    Some((None, *index))
                } else {
                    if let Some((_, index)) = elements.search_id(search_id) {
                        Some((None, index))
                    } else {
                        None
                    }
                }
            }
        }
    }

    fn number_of_flows(&self) -> usize {
        0
    }

    fn all_elements_ref(&self) -> Vec<&BPMNElement> {
        vec![self]
    }

    fn element_mut(&mut self, index: usize) -> Option<&mut BPMNElement> {
        if self.index() == index {
            Some(self)
        } else {
            None
        }
    }
}

impl BPMNObject for BPMNElement {
    fn index(&self) -> usize {
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
            | BPMNElement::ParallelGateway { index, .. }
            | BPMNElement::EventBasedGateway { index, .. }
            | BPMNElement::ExpandedSubProcess { index, .. }
            | BPMNElement::CollapsedSubProcess { index, .. }
            | BPMNElement::InclusiveGateway { index, .. } => *index,
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
            | BPMNElement::ParallelGateway { id, .. }
            | BPMNElement::EventBasedGateway { id, .. }
            | BPMNElement::ExpandedSubProcess { id, .. }
            | BPMNElement::CollapsedSubProcess { id, .. }
            | BPMNElement::InclusiveGateway { id, .. } => id,
        }
    }

    fn can_have_incoming_sequence_flow(&self) -> bool {
        match self {
            BPMNElement::StartEvent { .. } => false,
            BPMNElement::MessageStartEvent { .. } => false,
            _ => true,
        }
    }

    fn can_catch_message(&self) -> bool {
        match self {
            BPMNElement::MessageIntermediateCatchEvent { .. }
            | BPMNElement::MessageStartEvent { .. }
            | BPMNElement::CollapsedSubProcess { .. }
            | BPMNElement::Task { .. } => true,
            _ => false,
        }
    }

    fn can_throw_message(&self) -> bool {
        match self {
            BPMNElement::MessageIntermediateThrowEvent { .. }
            | BPMNElement::MessageEndEvent { .. }
            | BPMNElement::CollapsedSubProcess { .. }
            | BPMNElement::Task { .. } => true,
            _ => false,
        }
    }
}
