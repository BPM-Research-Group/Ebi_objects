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

pub struct EventBasedGateway {}

impl Recognisable for EventBasedGateway {
    fn recognise_tag(e: &BytesStart, state: &ParserState) -> Option<Tag>
    where
        Self: Sized,
    {
        if state.open_tags.len() >= 1 {
            if let Some(OpenedTag::Process { .. }) = state.open_tags.get(state.open_tags.len() - 1)
            {
                {
                    if e.local_name().as_ref() == b"eventBasedGateway" {
                        return Some(Tag::EventBasedGateway);
                    }
                }
            }
        }
        None
    }
}

impl Openable for EventBasedGateway {
    fn open_tag(_tag: Tag, e: &BytesStart, state: &mut ParserState) -> Result<OpenedTag>
    where
        Self: Sized,
    {
        let (index, id) = state.read_and_add_id(e)?;

        Ok(OpenedTag::EventBasedGateway { index, id })
    }
}

impl Closeable for EventBasedGateway {
    fn close_tag(opened_tag: OpenedTag, _e: &BytesEnd, state: &mut ParserState) -> Result<()> {
        let index = state.open_tags.len() - 1;
        if let Some(OpenedTag::Process { elements, .. }) = state.open_tags.get_mut(index) {
            if let OpenedTag::EventBasedGateway { index, id } = opened_tag {
                elements.push(BPMNElement::EventBasedGateway { index, id });
                Ok(())
            } else {
                unreachable!()
            }
        } else {
            unreachable!()
        }
    }
}
