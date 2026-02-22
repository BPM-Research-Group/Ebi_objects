use crate::bpmn::{
    element::BPMNElement,
    importer::parse_attribute,
    parser::{
        parser_state::ParserState,
        parser_traits::{Closeable, Openable, Recognisable},
        tags::{OpenedTag, Tag},
    },
};
use anyhow::Result;
use quick_xml::events::{BytesEnd, BytesStart};

pub struct Task {}

impl Recognisable for Task {
    fn recognise_tag(e: &BytesStart, state: &ParserState) -> Option<Tag>
    where
        Self: Sized,
    {
        match state.open_tags.iter().last() {
            Some(OpenedTag::Process { .. }) | Some(OpenedTag::SubProcess { .. }) => {
                if e.local_name().as_ref() == b"task" {
                    return Some(Tag::Task);
                }
            }
            _ => {}
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

        let label = parse_attribute(e, "name").unwrap_or_else(|| String::new());
        let activity = state.activity_key.process_activity(&label);
        Ok(OpenedTag::Task {
            index,
            id,
            activity,
        })
    }
}

impl Closeable for Task {
    fn close_tag(opened_tag: OpenedTag, _e: &BytesEnd, state: &mut ParserState) -> Result<()> {
        match state.open_tags.iter_mut().last() {
            Some(OpenedTag::Process { elements, .. })
            | Some(OpenedTag::SubProcess { elements, .. }) => {
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
                        outgoing_sequence_flows: vec![],
                    });
                    Ok(())
                } else {
                    unreachable!()
                }
            }
            _ => unreachable!(),
        }
    }
}
