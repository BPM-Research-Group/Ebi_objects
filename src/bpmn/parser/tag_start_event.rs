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

pub struct StartEvent {}

impl Recognisable for StartEvent {
    fn recognise_tag(e: &BytesStart, state: &ParserState) -> Option<Tag>
    where
        Self: Sized,
    {
        if state.open_tags.len() >= 1 {
            if let Some(OpenedTag::Process { .. }) = state.open_tags.get(state.open_tags.len() - 1)
            {
                {
                    if e.local_name().as_ref() == b"startEvent" {
                        return Some(Tag::StartEvent);
                    }
                }
            }
        }
        None
    }
}

impl Openable for StartEvent {
    fn open_tag(_tag: Tag, e: &BytesStart, state: &mut ParserState) -> Result<OpenedTag>
    where
        Self: Sized,
    {
        let (index, id) = state.read_and_add_id(e)?;

        Ok(OpenedTag::StartEvent {
            index,
            id,
            message_index: None,
            message_id: None,
        })
    }
}

impl Closeable for StartEvent {
    fn close_tag(opened_tag: OpenedTag, _e: &BytesEnd, state: &mut ParserState) -> Result<()> {
        let index = state.open_tags.len() - 1;
        if let Some(OpenedTag::Process { elements, .. }) = state.open_tags.get_mut(index) {
            if let OpenedTag::StartEvent {
                index,
                id,
                message_index,
                message_id,
            } = opened_tag
            {
                if let (Some(message_index), Some(message_id)) = (message_index, message_id) {
                    elements.push(BPMNElement::MessageStartEvent {
                        index,
                        id,
                        message_index,
                        message_id,
                    });
                } else {
                    elements.push(BPMNElement::StartEvent { index, id });
                }
                Ok(())
            } else {
                unreachable!()
            }
        } else {
            unreachable!()
        }
    }
}
