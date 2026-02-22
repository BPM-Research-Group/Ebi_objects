use crate::{
    Activity,
    bpmn::objects::{BPMNObject, IdSearchable},
};
use strum_macros::EnumIs;

static EMPTY_SEQUENCE_FLOWS: Vec<usize> = vec![];

#[derive(Clone, Debug, EnumIs)]
pub enum BPMNElement {
    EndEvent {
        index: usize,
        id: String,
    },
    EventBasedGateway {
        index: usize,
        id: String,
        outgoing_sequence_flows: Vec<usize>,
    },
    ExclusiveGateway {
        index: usize,
        id: String,
        outgoing_sequence_flows: Vec<usize>,
    },
    InclusiveGateway {
        index: usize,
        id: String,
        outgoing_sequence_flows: Vec<usize>,
    },
    IntermediateCatchEvent {
        index: usize,
        id: String,
        outgoing_sequence_flows: Vec<usize>,
    },
    IntermediateThrowEvent {
        index: usize,
        id: String,
        outgoing_sequence_flows: Vec<usize>,
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
        outgoing_sequence_flows: Vec<usize>,
    },
    MessageIntermediateThrowEvent {
        index: usize,
        id: String,
        message_index: usize,
        message_id: String,
        outgoing_sequence_flows: Vec<usize>,
    },
    MessageStartEvent {
        index: usize,
        id: String,
        message_index: usize,
        message_id: String,
        outgoing_sequence_flows: Vec<usize>,
    },
    ParallelGateway {
        index: usize,
        id: String,
        outgoing_sequence_flows: Vec<usize>,
    },
    StartEvent {
        index: usize,
        id: String,
        outgoing_sequence_flows: Vec<usize>,
    },
    ExpandedSubProcess {
        index: usize,
        id: String,
        name: Option<String>,
        elements: Vec<BPMNElement>,
        outgoing_sequence_flows: Vec<usize>,
    },
    CollapsedSubProcess {
        index: usize,
        id: String,
        name: Option<String>,
        outgoing_sequence_flows: Vec<usize>,
    },
    Task {
        index: usize,
        id: String,
        activity: Activity,
        outgoing_sequence_flows: Vec<usize>,
    },
}

impl BPMNElement {
    pub(crate) fn add_outgoing_sequence_flow(&mut self, flow_index: usize) -> bool {
        match self {
            BPMNElement::CollapsedSubProcess {
                outgoing_sequence_flows,
                ..
            }
            | BPMNElement::EventBasedGateway {
                outgoing_sequence_flows,
                ..
            }
            | BPMNElement::ExclusiveGateway {
                outgoing_sequence_flows,
                ..
            }
            | BPMNElement::ExpandedSubProcess {
                outgoing_sequence_flows,
                ..
            }
            | BPMNElement::InclusiveGateway {
                outgoing_sequence_flows,
                ..
            }
            | BPMNElement::IntermediateCatchEvent {
                outgoing_sequence_flows,
                ..
            }
            | BPMNElement::IntermediateThrowEvent {
                outgoing_sequence_flows,
                ..
            }
            | BPMNElement::MessageIntermediateCatchEvent {
                outgoing_sequence_flows,
                ..
            }
            | BPMNElement::MessageIntermediateThrowEvent {
                outgoing_sequence_flows,
                ..
            }
            | BPMNElement::MessageStartEvent {
                outgoing_sequence_flows,
                ..
            }
            | BPMNElement::ParallelGateway {
                outgoing_sequence_flows,
                ..
            }
            | BPMNElement::StartEvent {
                outgoing_sequence_flows,
                ..
            }
            | BPMNElement::Task {
                outgoing_sequence_flows,
                ..
            } => {
                outgoing_sequence_flows.push(flow_index);
                true
            }
            BPMNElement::EndEvent { .. } | BPMNElement::MessageEndEvent { .. } => false,
        }
    }
}

impl IdSearchable for BPMNElement {
    fn index_2_object(&self, search_index: usize) -> Option<&dyn BPMNObject> {
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
                    elements.index_2_object(search_index)
                }
            }
        }
    }

    fn id_2_pool_and_index(&self, search_id: &str) -> Option<(Option<usize>, usize)> {
        match self {
            BPMNElement::StartEvent { index, id, .. }
            | BPMNElement::IntermediateThrowEvent { index, id, .. }
            | BPMNElement::IntermediateCatchEvent { index, id, .. }
            | BPMNElement::EndEvent { index, id, .. }
            | BPMNElement::CollapsedSubProcess { index, id, .. }
            | BPMNElement::ExclusiveGateway { index, id, .. }
            | BPMNElement::InclusiveGateway { index, id, .. }
            | BPMNElement::EventBasedGateway { index, id, .. }
            | BPMNElement::Task { index, id, .. }
            | BPMNElement::ParallelGateway { index, id, .. } => {
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
                ..
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
                ..
            }
            | BPMNElement::MessageIntermediateCatchEvent {
                index,
                id,
                message_index,
                message_id,
                ..
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
                    if let Some((_, index)) = elements.id_2_pool_and_index(search_id) {
                        Some((None, index))
                    } else {
                        None
                    }
                }
            }
        }
    }

    fn all_elements_ref(&self) -> Vec<&BPMNElement> {
        vec![self]
    }

    fn index_2_element_mut(&mut self, index: usize) -> Option<&mut BPMNElement> {
        if self.index() == index {
            Some(self)
        } else if let BPMNElement::ExpandedSubProcess { elements, .. } = self {
            elements.index_2_element_mut(index)
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

    fn outgoing_sequence_flows(&self) -> &Vec<usize> {
        match self {
            BPMNElement::StartEvent {
                outgoing_sequence_flows,
                ..
            }
            | BPMNElement::IntermediateThrowEvent {
                outgoing_sequence_flows,
                ..
            }
            | BPMNElement::IntermediateCatchEvent {
                outgoing_sequence_flows,
                ..
            }
            | BPMNElement::Task {
                outgoing_sequence_flows,
                ..
            }
            | BPMNElement::ExclusiveGateway {
                outgoing_sequence_flows,
                ..
            }
            | BPMNElement::MessageStartEvent {
                outgoing_sequence_flows,
                ..
            }
            | BPMNElement::MessageIntermediateCatchEvent {
                outgoing_sequence_flows,
                ..
            }
            | BPMNElement::MessageIntermediateThrowEvent {
                outgoing_sequence_flows,
                ..
            }
            | BPMNElement::ParallelGateway {
                outgoing_sequence_flows,
                ..
            }
            | BPMNElement::EventBasedGateway {
                outgoing_sequence_flows,
                ..
            }
            | BPMNElement::ExpandedSubProcess {
                outgoing_sequence_flows,
                ..
            }
            | BPMNElement::CollapsedSubProcess {
                outgoing_sequence_flows,
                ..
            }
            | BPMNElement::InclusiveGateway {
                outgoing_sequence_flows,
                ..
            } => outgoing_sequence_flows,
            BPMNElement::MessageEndEvent { .. } | BPMNElement::EndEvent { .. } => {
                &EMPTY_SEQUENCE_FLOWS
            }
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
