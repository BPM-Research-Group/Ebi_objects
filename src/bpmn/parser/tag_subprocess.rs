use crate::bpmn::{
    element::BPMNElement,
    importer::parse_attribute,
    parser::{
        parser_state::ParserState,
        parser_traits::{Closeable, Openable, Recognisable},
        tag_process::process_sequence_flows,
        tags::{OpenedTag, Tag},
    },
};
use anyhow::Result;
use quick_xml::events::{BytesEnd, BytesStart};

pub struct SubProcess {}

impl Recognisable for SubProcess {
    fn recognise_tag(e: &BytesStart, state: &ParserState) -> Option<Tag>
    where
        Self: Sized,
    {
        match state.open_tags.iter().last() {
            Some(OpenedTag::Process { .. }) | Some(OpenedTag::SubProcess { .. }) => {
                if e.local_name().as_ref() == b"subProcess" {
                    return Some(Tag::SubProcess);
                }
            }
            _ => {}
        }
        None
    }
}

impl Openable for SubProcess {
    fn open_tag(_tag: Tag, e: &BytesStart, state: &mut ParserState) -> Result<OpenedTag>
    where
        Self: Sized,
    {
        let (index, id) = state.read_and_add_id(e)?;

        let name = parse_attribute(e, "name");
        Ok(OpenedTag::SubProcess {
            index,
            id,
            name,
            elements: vec![],
            draft_sequence_flows: vec![],
        })
    }
}

impl Closeable for SubProcess {
    fn close_tag(opened_tag: OpenedTag, _e: &BytesEnd, state: &mut ParserState) -> Result<()> {
        if let OpenedTag::SubProcess {
            index,
            id,
            name,
            elements,
            draft_sequence_flows,
        } = opened_tag
        {
            let sequence_flows = process_sequence_flows(state, draft_sequence_flows)?;

            match state.open_tags.iter_mut().last() {
                Some(OpenedTag::Process {
                    elements: super_elements,
                    ..
                })
                | Some(OpenedTag::SubProcess {
                    elements: super_elements,
                    ..
                }) => {
                    if elements.is_empty() {
                        //create a collapsed sub-process
                        super_elements.push(BPMNElement::CollapsedSubProcess { index, id, name });
                    } else {
                        //create an expanded process
                        super_elements.push(BPMNElement::ExpandedSubProcess {
                            index,
                            id,
                            name,
                            elements,
                            sequence_flows,
                        });
                    }
                    Ok(())
                }
                _ => unreachable!(),
            }
        } else {
            unreachable!()
        }
    }
}
