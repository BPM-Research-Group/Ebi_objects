use crate::bpmn::{
    element::BPMNElement,
    elements::event_based_gateway::BPMNEventBasedGateway,
    parser::{
        parser_state::ParserState,
        parser_traits::{Closeable, Openable, Recognisable},
        tags::{OpenedTag, Tag},
    },
};
use anyhow::Result;
use quick_xml::events::{BytesEnd, BytesStart};

pub struct TagEventBasedGateway {}

impl Recognisable for TagEventBasedGateway {
    fn recognise_tag(e: &BytesStart, state: &ParserState) -> Option<Tag>
    where
        Self: Sized,
    {
        match state.open_tags.iter().last() {
            Some(OpenedTag::Process { .. }) | Some(OpenedTag::SubProcess { .. }) => {
                if e.local_name().as_ref() == b"eventBasedGateway" {
                    return Some(Tag::EventBasedGateway);
                }
            }
            _ => {}
        }
        None
    }
}

impl Openable for TagEventBasedGateway {
    fn open_tag(_tag: Tag, e: &BytesStart, state: &mut ParserState) -> Result<OpenedTag>
    where
        Self: Sized,
    {
        let (index, id) = state.read_and_add_id(e)?;

        Ok(OpenedTag::EventBasedGateway { index, id })
    }
}

impl Closeable for TagEventBasedGateway {
    fn close_tag(opened_tag: OpenedTag, _e: &BytesEnd, state: &mut ParserState) -> Result<()> {
        match state.open_tags.iter_mut().last() {
            Some(OpenedTag::Process { elements, .. })
            | Some(OpenedTag::SubProcess { elements, .. }) => {
                if let OpenedTag::EventBasedGateway { index, id } = opened_tag {
                    elements.push(BPMNElement::EventBasedGateway(BPMNEventBasedGateway {
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
