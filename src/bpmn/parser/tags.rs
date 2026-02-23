use crate::{
    Activity,
    bpmn::{
        element::BPMNElement,
        elements::collapsed_pool::BPMNCollapsedPool,
        parser::{
            parser_state::ParserState,
            parser_traits::{Closeable, Openable, Recognisable},
            tag_collaboration::Collaboration,
            tag_definitions::Definitions,
            tag_end_event::TagEndEvent,
            tag_event_based_gateway::TagEventBasedGateway,
            tag_exclusive_gateway::TagExclusiveGateway,
            tag_inclusive_gateway::TagInclusiveGateway,
            tag_intermediate_catch_event::TagIntermediateCatchEvent,
            tag_intermediate_throw_event::TagIntermediateThrowEvent,
            tag_message_event_definition::TagMessageEventDefinition,
            tag_message_flow::{DraftMessageFlow, TagMessageFlow},
            tag_parallel_gateway::TagParallelGateway,
            tag_participant::TagParticipant,
            tag_process::TagProcess,
            tag_sequence_flow::{DraftSequenceFlow, TagSequenceFlow},
            tag_start_event::TagStartEvent,
            tag_subprocess::TagSubProcess,
            tag_task::TagTask,
        },
        sequence_flow::SequenceFlow,
    },
};
use anyhow::Result;
use quick_xml::events::{BytesEnd, BytesStart};
use strum::IntoEnumIterator;
use strum_macros::{Display, EnumIs, EnumIter, EnumString};

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
    Participant,
    Process,
    SequenceFlow,
    SubProcess,
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
                Tag::Process => TagProcess::recognise_tag(e, state),
                Tag::Collaboration => Collaboration::recognise_tag(e, state),
                Tag::MessageEventDefinition => TagMessageEventDefinition::recognise_tag(e, state),
                Tag::MessageFlow => TagMessageFlow::recognise_tag(e, state),
                Tag::StartEvent => TagStartEvent::recognise_tag(e, state),
                Tag::Task => TagTask::recognise_tag(e, state),
                Tag::SequenceFlow => TagSequenceFlow::recognise_tag(e, state),
                Tag::EndEvent => TagEndEvent::recognise_tag(e, state),
                Tag::ExclusiveGateway => TagExclusiveGateway::recognise_tag(e, state),
                Tag::InclusiveGateway => TagInclusiveGateway::recognise_tag(e, state),
                Tag::IntermediateThrowEvent => TagIntermediateThrowEvent::recognise_tag(e, state),
                Tag::IntermediateCatchEvent => TagIntermediateCatchEvent::recognise_tag(e, state),
                Tag::ParallelGateway => TagParallelGateway::recognise_tag(e, state),
                Tag::EventBasedGateway => TagEventBasedGateway::recognise_tag(e, state),
                Tag::SubProcess => TagSubProcess::recognise_tag(e, state),
                Tag::Participant => TagParticipant::recognise_tag(e, state),
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
            Tag::Process => TagProcess::open_tag(tag, e, state),
            Tag::Collaboration => Collaboration::open_tag(tag, e, state),
            Tag::MessageEventDefinition => TagMessageEventDefinition::open_tag(tag, e, state),
            Tag::MessageFlow => TagMessageFlow::open_tag(tag, e, state),
            Tag::StartEvent => TagStartEvent::open_tag(tag, e, state),
            Tag::Task => TagTask::open_tag(tag, e, state),
            Tag::SequenceFlow => TagSequenceFlow::open_tag(tag, e, state),
            Tag::EndEvent => TagEndEvent::open_tag(tag, e, state),
            Tag::ExclusiveGateway => TagExclusiveGateway::open_tag(tag, e, state),
            Tag::InclusiveGateway => TagInclusiveGateway::open_tag(tag, e, state),
            Tag::IntermediateThrowEvent => TagIntermediateThrowEvent::open_tag(tag, e, state),
            Tag::IntermediateCatchEvent => TagIntermediateCatchEvent::open_tag(tag, e, state),
            Tag::ParallelGateway => TagParallelGateway::open_tag(tag, e, state),
            Tag::EventBasedGateway => TagEventBasedGateway::open_tag(tag, e, state),
            Tag::SubProcess => TagSubProcess::open_tag(tag, e, state),
            Tag::Participant => TagParticipant::open_tag(tag, e, state),
        }
    }
}

#[derive(Debug, EnumIs)]
pub(crate) enum OpenedTag {
    Unknown,
    Collaboration {
        index: usize,
        id: String,
        collapsed_pools: Vec<BPMNCollapsedPool>,
        message_flows: Vec<DraftMessageFlow>,
    },
    Definitions {
        index: usize,
        id: String,
        collaboration_index: Option<usize>,
        collaboration_id: Option<String>,
        draft_message_flows: Vec<DraftMessageFlow>,
        sequence_flows: Vec<SequenceFlow>,
        elements: Vec<BPMNElement>,
    },
    EndEvent {
        index: usize,
        id: String,
        message_marker_index: Option<usize>,
        message_marker_id: Option<String>,
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
        message_marker_index: Option<usize>,
        message_marker_id: Option<String>,
    },
    IntermediateThrowEvent {
        index: usize,
        id: String,
        message_marker_index: Option<usize>,
        message_marker_id: Option<String>,
    },
    MessageEventDefinition {
        index: usize,
        id: String,
    },
    MessageFlow {
        index: usize,
        id: String,
        source_id: String,
        target_id: String,
    },
    ParallelGateway {
        index: usize,
        id: String,
    },
    Participant {
        index: usize,
        id: String,
        name: Option<String>,
        process_id: Option<String>,
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
    SubProcess {
        index: usize,
        id: String,
        name: Option<String>,
        elements: Vec<BPMNElement>,
        draft_sequence_flows: Vec<DraftSequenceFlow>,
    },
    StartEvent {
        index: usize,
        id: String,
        message_marker_index: Option<usize>,
        message_marker_id: Option<String>,
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
            OpenedTag::Process { .. } => TagProcess::close_tag(opened_tag, e, state),
            OpenedTag::Collaboration { .. } => Collaboration::close_tag(opened_tag, e, state),
            OpenedTag::MessageEventDefinition { .. } => {
                TagMessageEventDefinition::close_tag(opened_tag, e, state)
            }
            OpenedTag::MessageFlow { .. } => TagMessageFlow::close_tag(opened_tag, e, state),
            OpenedTag::StartEvent { .. } => TagStartEvent::close_tag(opened_tag, e, state),
            OpenedTag::Task { .. } => TagTask::close_tag(opened_tag, e, state),
            OpenedTag::SequenceFlow { .. } => TagSequenceFlow::close_tag(opened_tag, e, state),
            OpenedTag::EndEvent { .. } => TagEndEvent::close_tag(opened_tag, e, state),
            OpenedTag::ExclusiveGateway { .. } => {
                TagExclusiveGateway::close_tag(opened_tag, e, state)
            }
            OpenedTag::InclusiveGateway { .. } => {
                TagInclusiveGateway::close_tag(opened_tag, e, state)
            }
            OpenedTag::IntermediateThrowEvent { .. } => {
                TagIntermediateThrowEvent::close_tag(opened_tag, e, state)
            }
            OpenedTag::IntermediateCatchEvent { .. } => {
                TagIntermediateCatchEvent::close_tag(opened_tag, e, state)
            }
            OpenedTag::ParallelGateway { .. } => {
                TagParallelGateway::close_tag(opened_tag, e, state)
            }
            OpenedTag::EventBasedGateway { .. } => {
                TagEventBasedGateway::close_tag(opened_tag, e, state)
            }
            OpenedTag::SubProcess { .. } => TagSubProcess::close_tag(opened_tag, e, state),
            OpenedTag::Participant { .. } => TagParticipant::close_tag(opened_tag, e, state),
        }
    }
}
