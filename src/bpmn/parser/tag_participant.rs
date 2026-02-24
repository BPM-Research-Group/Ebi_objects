use crate::bpmn::{
    elements::{collapsed_pool::BPMNCollapsedPool, participant::BPMNParticipant},
    importer::parse_attribute,
    parser::{
        parser_state::ParserState,
        parser_traits::{Closeable, Openable, Recognisable},
        tags::{OpenedTag, Tag},
    },
};
use anyhow::Result;
use quick_xml::events::BytesStart;

pub(crate) struct TagParticipant {}

impl Recognisable for TagParticipant {
    fn recognise_tag(
        e: &quick_xml::events::BytesStart,
        state: &super::parser_state::ParserState,
    ) -> Option<super::tags::Tag>
    where
        Self: Sized,
    {
        match state.open_tags.iter().last() {
            Some(OpenedTag::Collaboration { .. }) => {
                if e.local_name().as_ref() == b"participant" {
                    return Some(Tag::Participant);
                }
            }
            _ => {}
        }
        None
    }
}

impl Openable for TagParticipant {
    fn open_tag(_tag: Tag, e: &BytesStart, state: &mut ParserState) -> Result<OpenedTag>
    where
        Self: Sized,
    {
        let (index, id) = state.read_and_add_id(e)?;

        let name = parse_attribute(e, "name");

        let process_id = parse_attribute(e, "processRef");

        Ok(OpenedTag::Participant {
            index,
            id,
            name,
            process_id,
        })
    }
}

impl Closeable for TagParticipant {
    fn close_tag(
        opened_tag: OpenedTag,
        _e: &quick_xml::events::BytesEnd,
        state: &mut ParserState,
    ) -> Result<()> {
        match state.open_tags.iter_mut().last() {
            Some(OpenedTag::Collaboration {
                collapsed_pools, ..
            }) => {
                if let OpenedTag::Participant {
                    index,
                    id,
                    name,
                    process_id,
                } = opened_tag
                {
                    if let Some(process_id) = process_id {
                        //this is an expanded pool
                        //not a BPMN element (that's the process), but we need to keep track of it anyway
                        state.participants.push(BPMNParticipant {
                            index,
                            id,
                            name, 
                            process_id,
                        });
                    } else {
                        //this is a collapsed pool
                        collapsed_pools.push(BPMNCollapsedPool {
                            index,
                            id,
                            name,
                            incoming_message_flows: vec![],
                            outgoing_message_flows: vec![],
                        });
                    }
                } else {
                    unreachable!()
                }
            }
            _ => unreachable!(),
        }
        Ok(())
    }
}
