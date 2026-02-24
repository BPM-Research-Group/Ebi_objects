use crate::bpmn::parser::{
    parser_state::ParserState,
    parser_traits::{Closeable, Openable, Recognisable},
    tags::{OpenedTag, Tag},
};
use anyhow::Result;
use quick_xml::events::{BytesEnd, BytesStart};

pub(crate) struct TagMessageEventDefinition {}

impl Recognisable for TagMessageEventDefinition {
    fn recognise_tag(
        e: &quick_xml::events::BytesStart,
        state: &super::parser_state::ParserState,
    ) -> Option<super::tags::Tag>
    where
        Self: Sized,
    {
        match state.open_tags.iter().last() {
            Some(OpenedTag::StartEvent { .. })
            | Some(OpenedTag::EndEvent { .. })
            | Some(OpenedTag::IntermediateCatchEvent { .. })
            | Some(OpenedTag::IntermediateThrowEvent { .. }) => {
                if e.local_name().as_ref() == b"messageEventDefinition" {
                    return Some(Tag::MessageEventDefinition);
                }
            }
            _ => {}
        }
        None
    }
}

impl Openable for TagMessageEventDefinition {
    fn open_tag(_tag: Tag, e: &BytesStart, state: &mut ParserState) -> Result<OpenedTag>
    where
        Self: Sized,
    {
        let (_, id) = state.read_and_add_id(e)?;

        Ok(OpenedTag::MessageEventDefinition { id })
    }
}

impl Closeable for TagMessageEventDefinition {
    fn close_tag(opened_tag: OpenedTag, _e: &BytesEnd, state: &mut ParserState) -> Result<()> {
        let index = state.open_tags.len() - 1;
        match state.open_tags.get_mut(index) {
            Some(OpenedTag::StartEvent {
                message_marker_id: message_id,
                ..
            })
            | Some(OpenedTag::EndEvent {
                message_marker_id: message_id,
                ..
            })
            | Some(OpenedTag::IntermediateCatchEvent {
                message_marker_id: message_id,
                ..
            })
            | Some(OpenedTag::IntermediateThrowEvent {
                message_marker_id: message_id,
                ..
            }) => {
                if let OpenedTag::MessageEventDefinition { id } = opened_tag {
                    *message_id = Some(id);
                    Ok(())
                } else {
                    unreachable!()
                }
            }
            _ => unreachable!(),
        }
    }
}
