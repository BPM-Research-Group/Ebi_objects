use crate::bpmn::parser::{
    parser_state::ParserState,
    parser_traits::{Closeable, Openable, Recognisable},
    tags::{OpenedTag, Tag},
};
use anyhow::{Result, anyhow};
use quick_xml::events::{BytesEnd, BytesStart};

pub(crate) struct Collaboration {}

impl Recognisable for Collaboration {
    fn recognise_tag(e: &BytesStart, state: &ParserState) -> Option<Tag>
    where
        Self: Sized,
    {
        if state.open_tags.len() >= 1 {
            if let Some(OpenedTag::Definitions { .. }) =
                state.open_tags.get(state.open_tags.len() - 1)
            {
                {
                    if e.local_name().as_ref() == b"collaboration" {
                        return Some(Tag::Collaboration);
                    }
                }
            }
        }
        None
    }
}

impl Openable for Collaboration {
    fn open_tag(_tag: Tag, e: &BytesStart, state: &mut ParserState) -> Result<OpenedTag>
    where
        Self: Sized,
    {
        let (index, id) = state.read_and_add_id(e)?;

        Ok(OpenedTag::Collaboration {
            index,
            id,
            collapsed_pools: vec![],
            message_flows: vec![],
        })
    }
}

impl Closeable for Collaboration {
    fn close_tag(opened_tag: OpenedTag, _e: &BytesEnd, state: &mut ParserState) -> Result<()> {
        let index = state.open_tags.len() - 1;
        if let Some(OpenedTag::Definitions {
            draft_message_flows: message_flows,
            collapsed_pools,
            collaboration_index,
            collaboration_id,
            ..
        }) = state.open_tags.get_mut(index)
        {
            if let OpenedTag::Collaboration {
                index,
                id,
                collapsed_pools: sub_collapsed_pools,
                message_flows: sub_message_flows,
            } = opened_tag
            {
                //verify that there is only one collaboration
                if collaboration_index.is_some() {
                    return Err(anyhow!("second collaboration found"));
                }

                *collaboration_index = Some(index);
                *collaboration_id = Some(id);

                message_flows.extend(sub_message_flows);
                collapsed_pools.extend(sub_collapsed_pools);
                Ok(())
            } else {
                unreachable!()
            }
        } else {
            unreachable!()
        }
    }
}
