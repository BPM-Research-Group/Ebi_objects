use crate::{
    BusinessProcessModelAndNotation,
    bpmn::{
        elements::{
            collapsed_pool::BPMNCollapsedPool, collapsed_sub_process::BPMNCollapsedSubProcess,
            end_event::BPMNEndEvent, event_based_gateway::BPMNEventBasedGateway,
            exclusive_gateway::BPMNExclusiveGateway, expanded_sub_process::BPMNExpandedSubProcess,
            inclusive_gateway::BPMNInclusiveGateway,
            intermediate_catch_event::BPMNIntermediateCatchEvent,
            intermediate_throw_event::BPMNIntermediateThrowEvent,
            message_end_event::BPMNMessageEndEvent,
            message_intermediate_catch_event::BPMNMessageIntermediateCatchEvent,
            message_intermediate_throw_event::BPMNMessageIntermediateThrowEvent,
            message_start_event::BPMNMessageStartEvent, parallel_gateway::BPMNParallelGateway,
            process::BPMNProcess, start_event::BPMNStartEvent, task::BPMNTask,
        },
        objects_elementable::Elementable,
        objects_objectable::BPMNObject,
        objects_searchable::Searchable,
        objects_transitionable::Transitionable,
        semantics::SemState,
    },
};
use anyhow::Result;
use bitvec::vec::BitVec;
use strum_macros::EnumIs;

#[derive(Clone, Debug, EnumIs)]
pub enum BPMNElement {
    CollapsedPool(BPMNCollapsedPool),
    CollapsedSubProcess(BPMNCollapsedSubProcess),
    EndEvent(BPMNEndEvent),
    EventBasedGateway(BPMNEventBasedGateway),
    ExclusiveGateway(BPMNExclusiveGateway),
    ExpandedSubProcess(BPMNExpandedSubProcess),
    InclusiveGateway(BPMNInclusiveGateway),
    IntermediateCatchEvent(BPMNIntermediateCatchEvent),
    IntermediateThrowEvent(BPMNIntermediateThrowEvent),
    MessageEndEvent(BPMNMessageEndEvent),
    MessageIntermediateCatchEvent(BPMNMessageIntermediateCatchEvent),
    MessageIntermediateThrowEvent(BPMNMessageIntermediateThrowEvent),
    MessageStartEvent(BPMNMessageStartEvent),
    ParallelGateway(BPMNParallelGateway),
    Process(BPMNProcess),
    StartEvent(BPMNStartEvent),
    Task(BPMNTask),
}

pub trait BPMNElementTrait {
    ///verify that structural requirements specific to this element are fulfilled
    fn verify_structural_correctness(&self, bpmn: &BusinessProcessModelAndNotation) -> Result<()>;

    ///Add an incoming sequence flow to the element. Returns whether successful.
    fn add_incoming_sequence_flow(&mut self, flow_index: usize) -> Result<()>;

    ///Add an outgoing sequence flow to the element. Returns whether successful.
    fn add_outgoing_sequence_flow(&mut self, flow_index: usize) -> Result<()>;

    ///Add an incoming message flow to the element. Returns whether successful.
    fn add_incoming_message_flow(&mut self, flow_index: usize) -> Result<()>;

    ///Add an outgoing message flow to the element. Returns whether successful.
    fn add_outgoing_message_flow(&mut self, flow_index: usize) -> Result<()>;

    // Extend `result` by pushing whether each transition is enabled.
    // fn enabled_transitions(&self, state: &SemState, result: &mut BitVec);
}

macro_rules! enums {
    ($self:ident, $fn:ident, $($v:ident),*) => {
        match $self {
            BPMNElement::CollapsedPool(x) => BPMNCollapsedPool::$fn(x, $($v),*),
            BPMNElement::CollapsedSubProcess(x) => BPMNCollapsedSubProcess::$fn(x, $($v),*),
            BPMNElement::EndEvent(x) => BPMNEndEvent::$fn(x, $($v),*),
            BPMNElement::EventBasedGateway(x) => BPMNEventBasedGateway::$fn(x, $($v),*),
            BPMNElement::ExclusiveGateway(x) => BPMNExclusiveGateway::$fn(x, $($v),*),
            BPMNElement::ExpandedSubProcess(x) => BPMNExpandedSubProcess::$fn(x, $($v),*),
            BPMNElement::InclusiveGateway(x) => BPMNInclusiveGateway::$fn(x, $($v),*),
            BPMNElement::IntermediateCatchEvent(x) => BPMNIntermediateCatchEvent::$fn(x, $($v),*),
            BPMNElement::IntermediateThrowEvent(x) => BPMNIntermediateThrowEvent::$fn(x, $($v),*),
            BPMNElement::MessageEndEvent(x) => BPMNMessageEndEvent::$fn(x, $($v),*),
            BPMNElement::MessageIntermediateCatchEvent(x) => {
                BPMNMessageIntermediateCatchEvent::$fn(x, $($v),*)
            }
            BPMNElement::MessageIntermediateThrowEvent(x) => {
                BPMNMessageIntermediateThrowEvent::$fn(x, $($v),*)
            }
            BPMNElement::MessageStartEvent(x) => BPMNMessageStartEvent::$fn(x, $($v),*),
            BPMNElement::ParallelGateway(x) => BPMNParallelGateway::$fn(x, $($v),*),
            BPMNElement::Process(x) => BPMNProcess::$fn(x, $($v),*),
            BPMNElement::StartEvent(x) => BPMNStartEvent::$fn(x, $($v),*),
            BPMNElement::Task(x) => BPMNTask::$fn(x, $($v),*),
        }
    };
}

impl BPMNElementTrait for BPMNElement {
    fn add_incoming_sequence_flow(&mut self, flow_index: usize) -> Result<()> {
        enums!(self, add_incoming_sequence_flow, flow_index)
    }

    fn add_outgoing_sequence_flow(&mut self, flow_index: usize) -> Result<()> {
        enums!(self, add_outgoing_sequence_flow, flow_index)
    }

    fn add_incoming_message_flow(&mut self, flow_index: usize) -> Result<()> {
        enums!(self, add_incoming_message_flow, flow_index)
    }

    fn add_outgoing_message_flow(&mut self, flow_index: usize) -> Result<()> {
        enums!(self, add_outgoing_message_flow, flow_index)
    }

    fn verify_structural_correctness(&self, bpmn: &BusinessProcessModelAndNotation) -> Result<()> {
        enums!(self, verify_structural_correctness, bpmn)
    }
}

impl BPMNElement {
    /// Extend `result` by pushing whether each transition is enabled.
    pub fn enabled_transitions(&self, _state: &SemState, _result: &mut BitVec) {
        todo!()
    }
}

impl Searchable for BPMNElement {
    fn index_2_object(&self, search_index: usize) -> Option<&dyn BPMNObject> {
        if self.index() == search_index {
            Some(self)
        } else if let BPMNElement::ExpandedSubProcess(BPMNExpandedSubProcess { elements, .. })
        | BPMNElement::Process(BPMNProcess { elements, .. }) = self
        {
            elements.index_2_object(search_index)
        } else {
            None
        }
    }

    fn id_2_pool_and_index(&self, search_id: &str) -> Option<(Option<usize>, usize)> {
        if self.id() == search_id && self.is_collapsed_pool() {
            Some((Some(self.index()), self.index()))
        } else if self.id() == search_id {
            Some((None, self.index()))
        } else if let BPMNElement::ExpandedSubProcess(BPMNExpandedSubProcess { elements, .. })
        | BPMNElement::Process(BPMNProcess { elements, .. }) = self
        {
            if let Some((_, index)) = elements.id_2_pool_and_index(search_id) {
                Some((Some(self.index()), index))
            } else {
                None
            }
        } else {
            None
        }
    }
}

impl Transitionable for BPMNElement {
    fn number_of_transitions(&self) -> usize {
        enums!(self, number_of_transitions,)
    }
}

impl Elementable for BPMNElement {
    fn all_elements_ref(&self) -> Vec<&BPMNElement> {
        if let BPMNElement::ExpandedSubProcess(BPMNExpandedSubProcess { elements, .. })
        | BPMNElement::Process(BPMNProcess { elements, .. }) = self
        {
            let mut result = elements
                .iter()
                .map(|element| element.all_elements_ref())
                .flatten()
                .collect::<Vec<_>>();
            result.push(self);
            result
        } else {
            vec![self]
        }
    }

    fn index_2_element(&self, index: usize) -> Option<&BPMNElement> {
        if self.index() == index {
            Some(self)
        } else if let BPMNElement::ExpandedSubProcess(BPMNExpandedSubProcess { elements, .. })
        | BPMNElement::Process(BPMNProcess { elements, .. }) = self
        {
            elements.index_2_element(index)
        } else {
            None
        }
    }

    fn index_2_element_mut(&mut self, index: usize) -> Option<&mut BPMNElement> {
        if self.index() == index {
            Some(self)
        } else if let BPMNElement::ExpandedSubProcess(BPMNExpandedSubProcess { elements, .. })
        | BPMNElement::Process(BPMNProcess { elements, .. }) = self
        {
            elements.index_2_element_mut(index)
        } else {
            None
        }
    }
}

impl BPMNObject for BPMNElement {
    fn index(&self) -> usize {
        enums!(self, index,)
    }

    fn id(&self) -> &str {
        enums!(self, id,)
    }

    fn incoming_sequence_flows(&self) -> &[usize] {
        enums!(self, incoming_sequence_flows,)
    }

    fn outgoing_sequence_flows(&self) -> &[usize] {
        enums!(self, outgoing_sequence_flows,)
    }

    fn incoming_message_flows(&self) -> &[usize] {
        enums!(self, incoming_message_flows,)
    }

    fn outgoing_message_flows(&self) -> &[usize] {
        enums!(self, outgoing_message_flows,)
    }

    fn can_have_incoming_sequence_flows(&self) -> bool {
        enums!(self, can_have_incoming_sequence_flows,)
    }
}
