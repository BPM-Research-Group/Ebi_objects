use crate::bpmn::{
    element::BPMNElement,
    elements::{
        intermediate_throw_event::BPMNIntermediateThrowEvent,
        message_intermediate_throw_event::BPMNMessageIntermediateThrowEvent,
    },
    parser::{
        parser_state::ParserState,
        parser_traits::{Closeable, Openable, Recognisable},
        tags::{OpenedTag, Tag},
    },
};
use anyhow::Result;
use quick_xml::events::{BytesEnd, BytesStart};

pub(crate) struct TagIntermediateThrowEvent {}

impl Recognisable for TagIntermediateThrowEvent {
    fn recognise_tag(e: &BytesStart, state: &ParserState) -> Option<Tag>
    where
        Self: Sized,
    {
        match state.open_tags.iter().last() {
            Some(OpenedTag::Process { .. }) | Some(OpenedTag::SubProcess { .. }) => {
                if e.local_name().as_ref() == b"intermediateThrowEvent" {
                    return Some(Tag::IntermediateThrowEvent);
                }
            }
            _ => {}
        }
        None
    }
}

impl Openable for TagIntermediateThrowEvent {
    fn open_tag(_tag: Tag, e: &BytesStart, state: &mut ParserState) -> Result<OpenedTag>
    where
        Self: Sized,
    {
        let (index, id) = state.read_and_add_id(e)?;

        Ok(OpenedTag::IntermediateThrowEvent {
            index,
            id,
            message_marker_id: None,
        })
    }
}

impl Closeable for TagIntermediateThrowEvent {
    fn close_tag(opened_tag: OpenedTag, _e: &BytesEnd, state: &mut ParserState) -> Result<()> {
        match state.open_tags.iter_mut().last() {
            Some(OpenedTag::Process { elements, .. })
            | Some(OpenedTag::SubProcess { elements, .. }) => {
                if let OpenedTag::IntermediateThrowEvent {
                    index,
                    id,
                    message_marker_id,
                } = opened_tag
                {
                    if let Some(message_marker_id) = message_marker_id {
                        elements.push(BPMNElement::MessageIntermediateThrowEvent(
                            BPMNMessageIntermediateThrowEvent {
                                index,
                                id,
                                message_marker_id,
                                incoming_sequence_flows: vec![],
                                outgoing_sequence_flows: vec![],
                                outgoing_message_flow: None,
                            },
                        ));
                    } else {
                        elements.push(BPMNElement::IntermediateThrowEvent(
                            BPMNIntermediateThrowEvent {
                                index,
                                id,
                                incoming_sequence_flows: vec![],
                                outgoing_sequence_flows: vec![],
                            },
                        ));
                    }
                    Ok(())
                } else {
                    unreachable!()
                }
            }
            _ => unreachable!(),
        }
    }
}
