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

pub(crate) struct SequenceFlow {}

impl Recognisable for SequenceFlow {
    fn recognise_tag(e: &BytesStart, state: &ParserState) -> Option<Tag>
    where
        Self: Sized,
    {
        match state.open_tags.iter().last() {
            Some(OpenedTag::Process { .. }) | Some(OpenedTag::SubProcess { .. }) => {
                if e.local_name().as_ref() == b"sequenceFlow" {
                    return Some(Tag::SequenceFlow);
                }
            }
            _ => {}
        }
        None
    }
}

impl Openable for SequenceFlow {
    fn open_tag(_tag: Tag, e: &BytesStart, state: &mut ParserState) -> anyhow::Result<OpenedTag>
    where
        Self: Sized,
    {
        let (index, id) = state.read_and_add_id(e)?;

        if let Some(source_ref) = parse_attribute(e, "sourceRef") {
            if let Some(target_ref) = parse_attribute(e, "targetRef") {
                Ok(OpenedTag::SequenceFlow {
                    index,
                    id,
                    source_ref,
                    target_ref,
                })
            } else {
                Err(anyhow!("sequence flow must have a target"))
            }
        } else {
            Err(anyhow!("sequence flow must have a source"))
        }
    }
}

impl Closeable for SequenceFlow {
    fn close_tag(
        opened_tag: OpenedTag,
        _e: &quick_xml::events::BytesEnd,
        state: &mut ParserState,
    ) -> Result<()> {
        match state.open_tags.iter_mut().last() {
            Some(OpenedTag::Process {
                draft_sequence_flows,
                ..
            })
            | Some(OpenedTag::SubProcess {
                draft_sequence_flows,
                ..
            }) => {
                if let OpenedTag::SequenceFlow {
                    index,
                    id,
                    source_ref,
                    target_ref,
                } = opened_tag
                {
                    draft_sequence_flows.push(DraftSequenceFlow {
                        index,
                        id,
                        source_ref,
                        target_ref,
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

#[derive(Clone, Debug)]
pub(crate) struct DraftSequenceFlow {
    pub(crate) index: usize,
    pub(crate) id: String,
    pub(crate) source_ref: String,
    pub(crate) target_ref: String,
}
