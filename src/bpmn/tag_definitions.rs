use crate::bpmn::{
    objects::{BPMNProcess, IdSearchable, MessageFlow},
    parser_state::ParserState,
    parser_traits::{Closeable, Openable, Recognisable},
    tag_message_flow::DraftMessageFlow,
    tags::{OpenedTag, Tag},
};
use anyhow::{Result, anyhow};
use quick_xml::events::{BytesEnd, BytesStart};

pub struct Definitions {}

impl Recognisable for Definitions {
    fn recognise_tag(e: &BytesStart, _state: &ParserState) -> Option<Tag>
    where
        Self: Sized,
    {
        if e.local_name().as_ref() == b"definitions" {
            return Some(Tag::Definitions);
        }
        None
    }
}

impl Openable for Definitions {
    fn open_tag(_tag: Tag, e: &BytesStart, state: &mut ParserState) -> Result<OpenedTag>
    where
        Self: Sized,
    {
        let (index, id) = state.read_and_add_id(e)?;

        Ok(OpenedTag::Definitions {
            index,
            id,
            collaboration_index: None,
            collaboration_id: None,
            draft_message_flows: vec![],
            processes: vec![],
        })
    }
}

impl Closeable for Definitions {
    fn close_tag(opened_tag: OpenedTag, _e: &BytesEnd, state: &mut ParserState) -> Result<()> {
        //finalise the message flows

        if let OpenedTag::Definitions {
            index,
            id,
            collaboration_index,
            collaboration_id,
            draft_message_flows,
            processes,
        } = opened_tag
        {
            //finalise the message flows
            let mut message_flows = Vec::with_capacity(draft_message_flows.len());
            for draft_message_flows in draft_message_flows {
                let DraftMessageFlow {
                    index,
                    id,
                    source_id,
                    target_id,
                } = draft_message_flows;
                if let Some((Some(source_process_rank), source_element_index)) =
                    processes.search_id(&source_id)
                {
                    if let Some((Some(target_process_rank), target_element_index)) =
                        processes.search_id(&target_id)
                    {
                        message_flows.push(MessageFlow {
                            index,
                            id,
                            source_element_index,
                            source_process_rank,
                            target_element_index,
                            target_process_rank,
                        });
                    } else {
                        return Err(anyhow!(
                            "id `{}` mentioned but not declared in message flow `{}`",
                            target_id,
                            id
                        ));
                    }
                } else {
                    return Err(anyhow!(
                        "id `{}` mentioned but not declared in message flow `{}`",
                        source_id,
                        id
                    ));
                }
            }

            state.draft_definitionss.push(DraftDefinitions {
                index,
                id,
                collaboration_index,
                collaboration_id,
                processes,
                message_flows,
            });

            Ok(())
        } else {
            unreachable!()
        }
    }
}

pub(crate) struct DraftDefinitions {
    pub(crate) index: usize,
    pub(crate) id: String,
    pub(crate) collaboration_index: Option<usize>,
    pub(crate) collaboration_id: Option<String>,
    pub(crate) processes: Vec<BPMNProcess>,
    pub(crate) message_flows: Vec<MessageFlow>,
}
