use crate::bpmn::{
    element::BPMNElement,
    elements::inclusive_gateway::BPMNInclusiveGateway,
    parser::{
        parser_state::ParserState,
        parser_traits::{Closeable, Openable, Recognisable},
        tags::{OpenedTag, Tag},
    },
};
use anyhow::Result;
use quick_xml::events::{BytesEnd, BytesStart};

pub struct TagInclusiveGateway {}

impl Recognisable for TagInclusiveGateway {
    fn recognise_tag(e: &BytesStart, state: &ParserState) -> Option<Tag>
    where
        Self: Sized,
    {
        match state.open_tags.iter().last() {
            Some(OpenedTag::Process { .. }) | Some(OpenedTag::SubProcess { .. }) => {
                if e.local_name().as_ref() == b"inclusiveGateway" {
                    return Some(Tag::InclusiveGateway);
                }
            }
            _ => {}
        }
        None
    }
}

impl Openable for TagInclusiveGateway {
    fn open_tag(_tag: Tag, e: &BytesStart, state: &mut ParserState) -> Result<OpenedTag>
    where
        Self: Sized,
    {
        let (index, id) = state.read_and_add_id(e)?;

        Ok(OpenedTag::InclusiveGateway { index, id })
    }
}

impl Closeable for TagInclusiveGateway {
    fn close_tag(opened_tag: OpenedTag, _e: &BytesEnd, state: &mut ParserState) -> Result<()> {
        match state.open_tags.iter_mut().last() {
            Some(OpenedTag::Process { elements, .. })
            | Some(OpenedTag::SubProcess { elements, .. }) => {
                if let OpenedTag::InclusiveGateway { index, id } = opened_tag {
                    elements.push(BPMNElement::InclusiveGateway(BPMNInclusiveGateway {
                        index,
                        id,
                        incoming_sequence_flows: vec![],
                        outgoing_sequence_flows: vec![],
                    }));
                    Ok(())
                } else {
                    unreachable!()
                }
            }
            _ => unreachable!(),
        }
    }
}
