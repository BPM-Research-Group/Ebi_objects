use crate::bpmn::{
    importer::parse_attribute,
    objects::BPMNElement,
    parser_state::ParserState,
    parser_traits::{Closeable, Openable, Recognisable},
    tags::{OpenedTag, Tag},
};
use anyhow::{Result, anyhow};
use quick_xml::events::{BytesEnd, BytesStart};

pub struct Task {}

impl Recognisable for Task {
    fn recognise_tag(e: &BytesStart, state: &ParserState) -> Option<Tag>
    where
        Self: Sized,
    {
        if state.open_tags.len() >= 1 {
            if let Some(OpenedTag::Process { .. }) = state.open_tags.get(state.open_tags.len() - 1)
            {
                {
                    if e.local_name().as_ref() == b"task" {
                        return Some(Tag::Task);
                    }
                }
            }
        }
        None
    }
}

impl Openable for Task {
    fn open_tag(_tag: Tag, e: &BytesStart, state: &mut ParserState) -> Result<OpenedTag>
    where
        Self: Sized,
    {
        let (index, id) = state.read_and_add_id(e)?;

        if let Some(label) = parse_attribute(e, "name") {
            let activity = state.activity_key.process_activity(&label);
            Ok(OpenedTag::Task {
                index,
                id,
                activity,
            })
        } else {
            Err(anyhow!("task must have a name"))
        }
    }
}

impl Closeable for Task {
    fn close_tag(opened_tag: OpenedTag, _e: &BytesEnd, state: &mut ParserState) -> Result<()> {
        let index = state.open_tags.len() - 1;
        if let Some(OpenedTag::Process { elements, .. }) = state.open_tags.get_mut(index) {
            if let OpenedTag::Task {
                index,
                id,
                activity,
            } = opened_tag
            {
                elements.push(BPMNElement::Task {
                    index,
                    id,
                    activity,
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
