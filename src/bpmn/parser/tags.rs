use crate::{
    Activity,
    bpmn::{
        element::BPMNElement,
        parser::{
            parser_state::ParserState,
            parser_traits::{Closeable, Openable, Recognisable},
            tag_collaboration::Collaboration,
            tag_definitions::Definitions,
            tag_end_event::EndEvent,
            tag_event_based_gateway::EventBasedGateway,
            tag_exclusive_gateway::ExclusiveGateway,
            tag_inclusive_gateway::InclusiveGateway,
            tag_intermediate_catch_event::IntermediateCatchEvent,
            tag_intermediate_throw_event::IntermediateThrowEvent,
            tag_message_event_definition::MessageEventDefinition,
            tag_message_flow::{DraftMessageFlow, MessageFlow},
            tag_parallel_gateway::ParallelGateway,
            tag_process::Process,
            tag_sequence_flow::{DraftSequenceFlow, SequenceFlow},
            tag_start_event::StartEvent,
            tag_task::Task,
        },
        process::BPMNProcess,
    },
};
use anyhow::Result;
use quick_xml::events::{BytesEnd, BytesStart};
use strum::IntoEnumIterator;
use strum_macros::{Display, EnumIter, EnumString};

#[derive(Clone, Copy, EnumString, EnumIter, Display)]
pub(crate) enum Tag {
    Collaboration,
    Definitions,
    EndEvent,
    EventBasedGateway,
    ExclusiveGateway,
    InclusiveGateway,
    IntermediateCatchEvent,
    IntermediateThrowEvent,
    MessageEventDefinition,
    MessageFlow,
    ParallelGateway,
    Process,
    SequenceFlow,
    StartEvent,
    Task,
}

impl Recognisable for Tag {
    fn recognise_tag(e: &BytesStart, state: &ParserState) -> Option<Self>
    where
        Self: Sized,
    {
        for tag in Tag::iter() {
            let x = match tag {
                Tag::Definitions => Definitions::recognise_tag(e, state),
                Tag::Process => Process::recognise_tag(e, state),
                Tag::Collaboration => Collaboration::recognise_tag(e, state),
                Tag::MessageEventDefinition => MessageEventDefinition::recognise_tag(e, state),
                Tag::MessageFlow => MessageFlow::recognise_tag(e, state),
                Tag::StartEvent => StartEvent::recognise_tag(e, state),
                Tag::Task => Task::recognise_tag(e, state),
                Tag::SequenceFlow => SequenceFlow::recognise_tag(e, state),
                Tag::EndEvent => EndEvent::recognise_tag(e, state),
                Tag::ExclusiveGateway => ExclusiveGateway::recognise_tag(e, state),
                Tag::InclusiveGateway => InclusiveGateway::recognise_tag(e, state),
                Tag::IntermediateThrowEvent => IntermediateThrowEvent::recognise_tag(e, state),
                Tag::IntermediateCatchEvent => IntermediateCatchEvent::recognise_tag(e, state),
                Tag::ParallelGateway => ParallelGateway::recognise_tag(e, state),
                Tag::EventBasedGateway => EventBasedGateway::recognise_tag(e, state),
            };
            if x.is_some() {
                return x;
            }
        }
        None
    }
}

impl Openable for Tag {
    fn open_tag(tag: Tag, e: &BytesStart, state: &mut ParserState) -> Result<OpenedTag>
    where
        Self: Sized,
    {
        match tag {
            Tag::Definitions => Definitions::open_tag(tag, e, state),
            Tag::Process => Process::open_tag(tag, e, state),
            Tag::Collaboration => Collaboration::open_tag(tag, e, state),
            Tag::MessageEventDefinition => MessageEventDefinition::open_tag(tag, e, state),
            Tag::MessageFlow => MessageFlow::open_tag(tag, e, state),
            Tag::StartEvent => StartEvent::open_tag(tag, e, state),
            Tag::Task => Task::open_tag(tag, e, state),
            Tag::SequenceFlow => SequenceFlow::open_tag(tag, e, state),
            Tag::EndEvent => EndEvent::open_tag(tag, e, state),
            Tag::ExclusiveGateway => ExclusiveGateway::open_tag(tag, e, state),
            Tag::InclusiveGateway => InclusiveGateway::open_tag(tag, e, state),
            Tag::IntermediateThrowEvent => IntermediateThrowEvent::open_tag(tag, e, state),
            Tag::IntermediateCatchEvent => IntermediateCatchEvent::open_tag(tag, e, state),
            Tag::ParallelGateway => ParallelGateway::open_tag(tag, e, state),
            Tag::EventBasedGateway => EventBasedGateway::open_tag(tag, e, state),
        }
    }
}

#[derive(Debug)]
pub(crate) enum OpenedTag {
    Unknown,
    Collaboration {
        index: usize,
        id: String,
        message_flows: Vec<DraftMessageFlow>,
    },
    Definitions {
        index: usize,
        id: String,
        collaboration_index: Option<usize>,
        collaboration_id: Option<String>,
        draft_message_flows: Vec<DraftMessageFlow>,
        processes: Vec<BPMNProcess>,
    },
    EndEvent {
        index: usize,
        id: String,
        message_index: Option<usize>,
        message_id: Option<String>,
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
        message_index: Option<usize>,
        message_id: Option<String>,
    },
    IntermediateThrowEvent {
        index: usize,
        id: String,
        message_index: Option<usize>,
        message_id: Option<String>,
    },
    MessageEventDefinition {
        index: usize,
        id: String,
    },
    MessageFlow {
        index: usize,
        id: String,
        source_ref: String,
        target_ref: String,
    },
    ParallelGateway {
        index: usize,
        id: String,
    },
    Process {
        index: usize,
        id: String,
        elements: Vec<BPMNElement>,
        draft_sequence_flows: Vec<DraftSequenceFlow>,
    },
    SequenceFlow {
        index: usize,
        id: String,
        source_ref: String,
        target_ref: String,
    },
    StartEvent {
        index: usize,
        id: String,
        message_index: Option<usize>,
        message_id: Option<String>,
    },
    Task {
        index: usize,
        id: String,
        activity: Activity,
    },
}

impl Closeable for OpenedTag {
    fn close_tag(opened_tag: OpenedTag, e: &BytesEnd, state: &mut ParserState) -> Result<()> {
        match opened_tag {
            OpenedTag::Unknown => Ok(()),
            OpenedTag::Definitions { .. } => Definitions::close_tag(opened_tag, e, state),
            OpenedTag::Process { .. } => Process::close_tag(opened_tag, e, state),
            OpenedTag::Collaboration { .. } => Collaboration::close_tag(opened_tag, e, state),
            OpenedTag::MessageEventDefinition { .. } => {
                MessageEventDefinition::close_tag(opened_tag, e, state)
            }
            OpenedTag::MessageFlow { .. } => MessageFlow::close_tag(opened_tag, e, state),
            OpenedTag::StartEvent { .. } => StartEvent::close_tag(opened_tag, e, state),
            OpenedTag::Task { .. } => Task::close_tag(opened_tag, e, state),
            OpenedTag::SequenceFlow { .. } => SequenceFlow::close_tag(opened_tag, e, state),
            OpenedTag::EndEvent { .. } => EndEvent::close_tag(opened_tag, e, state),
            OpenedTag::ExclusiveGateway { .. } => ExclusiveGateway::close_tag(opened_tag, e, state),
            OpenedTag::InclusiveGateway { .. } => InclusiveGateway::close_tag(opened_tag, e, state),
            OpenedTag::IntermediateThrowEvent { .. } => {
                IntermediateThrowEvent::close_tag(opened_tag, e, state)
            }
            OpenedTag::IntermediateCatchEvent { .. } => {
                IntermediateCatchEvent::close_tag(opened_tag, e, state)
            }
            OpenedTag::ParallelGateway { .. } => ParallelGateway::close_tag(opened_tag, e, state),
            OpenedTag::EventBasedGateway { .. } => {
                EventBasedGateway::close_tag(opened_tag, e, state)
            }
        }
    }
}
