use crate::bpmn::{
    importer::parse_attribute,
    parser::{
        parser_state::ParserState,
        parser_traits::{Closeable, Openable, Recognisable},
        tags::{OpenedTag, Tag},
    },
};
use anyhow::{Result, anyhow};
use quick_xml::events::BytesStart;

pub(crate) struct MessageFlow {}

impl Recognisable for MessageFlow {
    fn recognise_tag(e: &BytesStart, state: &ParserState) -> Option<Tag>
    where
        Self: Sized,
    {
        if state.open_tags.len() >= 1 {
            if let Some(OpenedTag::Collaboration { .. }) =
                state.open_tags.get(state.open_tags.len() - 1)
            {
                {
                    if e.local_name().as_ref() == b"messageFlow" {
                        return Some(Tag::MessageFlow);
                    }
                }
            }
        }
        None
    }
}

impl Openable for MessageFlow {
    fn open_tag(_tag: Tag, e: &BytesStart, state: &mut ParserState) -> Result<OpenedTag>
    where
        Self: Sized,
    {
        let (index, id) = state.read_and_add_id(e)?;

        if let Some(source_ref) = parse_attribute(e, "sourceRef") {
            if let Some(target_ref) = parse_attribute(e, "targetRef") {
                Ok(OpenedTag::MessageFlow {
                    index,
                    id,
                    source_ref,
                    target_ref,
                })
            } else {
                Err(anyhow!("message flow must have a target"))
            }
        } else {
            Err(anyhow!("message flow must have a source"))
        }
    }
}

impl Closeable for MessageFlow {
    fn close_tag(
        opened_tag: OpenedTag,
        _e: &quick_xml::events::BytesEnd,
        state: &mut ParserState,
    ) -> Result<()> {
        let index = state.open_tags.len() - 1;
        if let Some(OpenedTag::Collaboration { message_flows, .. }) = state.open_tags.get_mut(index)
        {
            if let OpenedTag::MessageFlow {
                index,
                id,
                source_ref,
                target_ref,
            } = opened_tag
            {
                message_flows.push(DraftMessageFlow {
                    index,
                    id,
                    source_id: source_ref,
                    target_id: target_ref,
                });
                Ok(())
            } else {
                unreachable!()
            }
        } else {
            unreachable!()
        }
    }
}

#[derive(Debug)]
pub(crate) struct DraftMessageFlow {
    pub(crate) index: usize,
    pub(crate) id: String,
    pub(crate) source_id: String,
    pub(crate) target_id: String,
}
