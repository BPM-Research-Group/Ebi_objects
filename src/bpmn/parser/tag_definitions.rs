use crate::bpmn::{
    element::{BPMNElement, BPMNElementTrait},
    message_flow::MessageFlow,
    objects_elementable::Elementable,
    objects_searchable::Searchable,
    parser::{
        parser_state::ParserState,
        parser_traits::{Closeable, Openable, Recognisable},
        tag_message_flow::DraftMessageFlow,
        tags::{OpenedTag, Tag},
    },
    sequence_flow::SequenceFlow,
};
use anyhow::{Context, Result, anyhow};
use quick_xml::events::{BytesEnd, BytesStart};

pub struct Definitions {}

impl Recognisable for Definitions {
    fn recognise_tag(e: &BytesStart, state: &ParserState) -> Option<Tag>
    where
        Self: Sized,
    {
        if state.open_tags.is_empty() {
            if e.local_name().as_ref() == b"definitions" {
                return Some(Tag::Definitions);
            }
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
            sequence_flows: vec![],
            elements: vec![],
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
            sequence_flows,
            mut elements,
        } = opened_tag
        {
            //finalise the message flows
            let mut message_flows = Vec::with_capacity(draft_message_flows.len());
            for draft_message_flows in draft_message_flows {
                let new_message_flow_index = message_flows.len();
                let DraftMessageFlow {
                    index,
                    id,
                    source_id,
                    target_id,
                } = draft_message_flows;

                //obtain source
                let (source_pool_index, source_element_index) =
                    elements.id_2_pool_and_index(&source_id).ok_or_else(|| {
                        anyhow!(
                            "could not find source `{}` of message flow `{}`",
                            source_id,
                            id
                        )
                    })?;

                //link to source element or pool
                if let Some(source) = elements.index_2_element_mut(source_element_index) {
                    //element
                    source
                        .add_outgoing_message_flow(new_message_flow_index)
                        .with_context(|| anyhow!("message flow `{}`", id))?;
                } else {
                    return Err(anyhow!(
                        "could not find source `{}` of message flow `{}`",
                        source_id,
                        id
                    ));
                }

                //obtain target
                let (target_pool_index, target_element_index) =
                    elements.id_2_pool_and_index(&target_id).ok_or_else(|| {
                        anyhow!(
                            "could not find target `{}` of message flow `{}`",
                            target_id,
                            id
                        )
                    })?;

                //link to target element or pool
                if let Some(target) = elements.index_2_element_mut(target_element_index) {
                    //element
                    target
                        .add_incoming_message_flow(new_message_flow_index)
                        .with_context(|| anyhow!("message flow `{}`", id))?;
                } else {
                    return Err(anyhow!(
                        "could not find target `{}` of message flow `{}`",
                        target_id,
                        id
                    ));
                }

                message_flows.push(MessageFlow {
                    index,
                    id,
                    source_element_index,
                    source_pool_index: source_pool_index
                        .ok_or_else(|| anyhow!("pool not found"))?,
                    target_element_index,
                    target_pool_index: target_pool_index
                        .ok_or_else(|| anyhow!("pool not found {}", target_id))?,
                });
            }

            state.draft_definitionss.push(DraftDefinitions {
                index,
                id,
                collaboration_index,
                collaboration_id,
                elements,
                message_flows,
                sequence_flows,
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
    pub(crate) elements: Vec<BPMNElement>,
    pub(crate) message_flows: Vec<MessageFlow>,
    pub(crate) sequence_flows: Vec<SequenceFlow>,
}
