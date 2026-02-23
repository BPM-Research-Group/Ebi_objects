use crate::bpmn::{
    element::BPMNElement,
    elements::{message_start_event::BPMNMessageStartEvent, start_event::BPMNStartEvent},
    parser::{
        parser_state::ParserState,
        parser_traits::{Closeable, Openable, Recognisable},
        tags::{OpenedTag, Tag},
    },
};
use anyhow::Result;
use quick_xml::events::{BytesEnd, BytesStart};

pub struct TagStartEvent {}

impl Recognisable for TagStartEvent {
    fn recognise_tag(e: &BytesStart, state: &ParserState) -> Option<Tag>
    where
        Self: Sized,
    {
        match state.open_tags.iter().last() {
            Some(OpenedTag::Process { .. }) | Some(OpenedTag::SubProcess { .. }) => {
                if e.local_name().as_ref() == b"startEvent" {
                    return Some(Tag::StartEvent);
                }
            }
            _ => {}
        }
        None
    }
}

impl Openable for TagStartEvent {
    fn open_tag(_tag: Tag, e: &BytesStart, state: &mut ParserState) -> Result<OpenedTag>
    where
        Self: Sized,
    {
        let (index, id) = state.read_and_add_id(e)?;

        Ok(OpenedTag::StartEvent {
            index,
            id,
            message_marker_index: None,
            message_marker_id: None,
        })
    }
}

impl Closeable for TagStartEvent {
    fn close_tag(opened_tag: OpenedTag, _e: &BytesEnd, state: &mut ParserState) -> Result<()> {
        match state.open_tags.iter_mut().last() {
            Some(OpenedTag::Process { elements, .. })
            | Some(OpenedTag::SubProcess { elements, .. }) => {
                if let OpenedTag::StartEvent {
                    index,
                    id,
                    message_marker_index,
                    message_marker_id,
                } = opened_tag
                {
                    if let (Some(message_marker_index), Some(message_marker_id)) =
                        (message_marker_index, message_marker_id)
                    {
                        elements.push(BPMNElement::MessageStartEvent(BPMNMessageStartEvent {
                            index,
                            id,
                            message_marker_index,
                            message_marker_id,
                            outgoing_sequence_flows: vec![],
                            incoming_message_flow: None,
                        }));
                    } else {
                        elements.push(BPMNElement::StartEvent(BPMNStartEvent {
                            index,
                            id,
                            outgoing_sequence_flows: vec![],
                        }));
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
