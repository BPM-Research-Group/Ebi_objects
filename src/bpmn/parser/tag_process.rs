use crate::bpmn::{
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
        if state.open_tags.len() >= 1 {
            if let Some(OpenedTag::Definitions { .. }) =
                state.open_tags.get(state.open_tags.len() - 1)
            {
                {
                    if e.local_name().as_ref() == b"process" {
                        return Some(Tag::Process);
                    }
                }
            }
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
        let index = state.open_tags.len() - 1;
        if let Some(OpenedTag::Definitions { processes, .. }) = state.open_tags.get_mut(index) {
            if let OpenedTag::Process {
                index,
                id,
                elements,
                draft_sequence_flows,
            } = opened_tag
            {
                //finalise the sequence flows
                let mut sequence_flows = Vec::with_capacity(draft_sequence_flows.len());
                for draft_sequence_flow in draft_sequence_flows {
                    let DraftSequenceFlow {
                        index,
                        id,
                        source_ref,
                        target_ref,
                    } = draft_sequence_flow;
                    let source_element_index = *state.ids.get(&source_ref).ok_or_else(|| {
                        anyhow!(
                            "id `{}` mentioned but not declared in sequence flow `{}`",
                            source_ref,
                            id
                        )
                    })?;
                    let target_element_index = *state.ids.get(&target_ref).ok_or_else(|| {
                        anyhow!(
                            "id `{}` mentioned but not declared in sequence flow `{}`",
                            target_ref,
                            id
                        )
                    })?;
                    sequence_flows.push(SequenceFlow {
                        index,
                        id,
                        source_element_index,
                        target_element_index,
                    });
                }

                //create a process
                processes.push(BPMNProcess {
                    index,
                    id,
                    elements,
                    sequence_flows,
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
