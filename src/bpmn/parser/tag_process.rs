use crate::bpmn::{
    objects::IdSearchable,
    parser::{
        parser_state::ParserState,
        parser_traits::{Closeable, Openable, Recognisable},
        tag_sequence_flow::DraftSequenceFlow,
        tags::{OpenedTag, Tag},
    },
    process::BPMNProcess,
    sequence_flow::SequenceFlow,
};
use anyhow::{Result, anyhow};
use quick_xml::events::{BytesEnd, BytesStart};

pub(crate) struct Process {}

impl Recognisable for Process {
    fn recognise_tag(e: &BytesStart, state: &ParserState) -> Option<Tag>
    where
        Self: Sized,
    {
        match state.open_tags.iter().last() {
            Some(OpenedTag::Definitions { .. }) => {
                if e.local_name().as_ref() == b"process" {
                    return Some(Tag::Process);
                }
            }
            _ => {}
        }
        None
    }
}

impl Openable for Process {
    fn open_tag(_tag: Tag, e: &BytesStart, state: &mut ParserState) -> Result<OpenedTag>
    where
        Self: Sized,
    {
        let (index, id) = state.read_and_add_id(e)?;

        Ok(OpenedTag::Process {
            index,
            id,
            elements: vec![],
            draft_sequence_flows: vec![],
        })
    }
}

impl Closeable for Process {
    fn close_tag(opened_tag: OpenedTag, _e: &BytesEnd, state: &mut ParserState) -> Result<()> {
        if let OpenedTag::Process {
            index,
            id,
            mut elements,
            draft_sequence_flows,
        } = opened_tag
        {
            if let Some(OpenedTag::Definitions {
                processes,
                sequence_flows,
                ..
            }) = state.open_tags.iter_mut().last()
            {
                //process sequence flows
                for draft_sequence_flow in draft_sequence_flows {
                    let DraftSequenceFlow {
                        index,
                        id,
                        source_id,
                        target_id,
                    } = draft_sequence_flow;
                    let new_flow_index = sequence_flows.len();
                    let source_index = elements
                        .id_2_pool_and_index(&source_id)
                        .ok_or_else(|| {
                            anyhow!(
                                "id `{}` mentioned but not declared in sequence flow `{}`",
                                source_id,
                                id
                            )
                        })?
                        .1;
                    //register the sequence flow in the source element
                    let source = elements.index_2_element_mut(source_index).ok_or_else(|| {
                        anyhow!(
                            "could not find object of id `{}` mentioned in sequence flow `{}`",
                            source_id,
                            id
                        )
                    })?;
                    if !source.add_outgoing_sequence_flow(new_flow_index) {
                        return Err(anyhow!(
                            "could not add sequence flow `{}` to element with id `{}`",
                            id,
                            source_id,
                        ));
                    }

                    let target_index = elements
                        .id_2_pool_and_index(&target_id)
                        .ok_or_else(|| {
                            anyhow!(
                                "id `{}` mentioned but not declared in sequence flow `{}`",
                                target_id,
                                id
                            )
                        })?
                        .1;

                    sequence_flows.push(SequenceFlow {
                        index,
                        id,
                        flow_index: new_flow_index,
                        source_index,
                        target_index,
                    });
                }

                //create a process
                processes.push(BPMNProcess {
                    index,
                    id,
                    elements,
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

#[macro_export]
macro_rules! process_draft_sequence_flows {
    () => {};
}
