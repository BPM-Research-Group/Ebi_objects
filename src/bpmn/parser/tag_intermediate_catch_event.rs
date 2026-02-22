use crate::bpmn::{
    element::BPMNElement,
    parser::{
        parser_state::ParserState,
        parser_traits::{Closeable, Openable, Recognisable},
        tags::{OpenedTag, Tag},
    },
};
use anyhow::Result;
use quick_xml::events::{BytesEnd, BytesStart};

pub(crate) struct IntermediateCatchEvent {}

impl Recognisable for IntermediateCatchEvent {
    fn recognise_tag(e: &BytesStart, state: &ParserState) -> Option<Tag>
    where
        Self: Sized,
    {
        match state.open_tags.iter().last() {
            Some(OpenedTag::Process { .. }) | Some(OpenedTag::SubProcess { .. }) => {
                if e.local_name().as_ref() == b"intermediateCatchEvent" {
                    return Some(Tag::IntermediateCatchEvent);
                }
            }
            _ => {}
        }
        None
    }
}

impl Openable for IntermediateCatchEvent {
    fn open_tag(_tag: Tag, e: &BytesStart, state: &mut ParserState) -> Result<OpenedTag>
    where
        Self: Sized,
    {
        let (index, id) = state.read_and_add_id(e)?;

        Ok(OpenedTag::IntermediateCatchEvent {
            index,
            id,
            message_index: None,
            message_id: None,
        })
    }
}

impl Closeable for IntermediateCatchEvent {
    fn close_tag(opened_tag: OpenedTag, _e: &BytesEnd, state: &mut ParserState) -> Result<()> {
        match state.open_tags.iter_mut().last() {
            Some(OpenedTag::Process { elements, .. })
            | Some(OpenedTag::SubProcess { elements, .. }) => {
                if let OpenedTag::IntermediateCatchEvent {
                    index,
                    id,
                    message_index,
                    message_id,
                } = opened_tag
                {
                    if let (Some(message_index), Some(message_id)) = (message_index, message_id) {
                        elements.push(BPMNElement::MessageIntermediateCatchEvent {
                            index,
                            id,
                            outgoing_sequence_flows: vec![],
                            message_index,
                            message_id,
                        });
                    } else {
                        elements.push(BPMNElement::IntermediateCatchEvent {
                            index,
                            id,
                            outgoing_sequence_flows: vec![],
                        });
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
