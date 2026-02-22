use crate::bpmn::{
    element::BPMNElement,
    importer::parse_attribute,
    objects::IdSearchable,
    parser::{
        parser_state::ParserState,
        parser_traits::{Closeable, Openable, Recognisable},
        tag_sequence_flow::DraftSequenceFlow,
        tags::{OpenedTag, Tag},
    },
    sequence_flow::SequenceFlow,
};
use anyhow::{Result, anyhow};
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
            mut elements,
            draft_sequence_flows,
        } = opened_tag
        {
            //first, process the sequence flows
            {
                //find the definitions tag
                if let Some(OpenedTag::Definitions { sequence_flows, .. }) =
                    state.open_tags.iter_mut().find(|tag| tag.is_definitions())
                {
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
                } else {
                    unreachable!()
                }
            }

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
                        super_elements.push(BPMNElement::CollapsedSubProcess {
                            index,
                            id,
                            name,
                            outgoing_sequence_flows: vec![],
                        });
                    } else {
                        //create an expanded sub-process

                        super_elements.push(BPMNElement::ExpandedSubProcess {
                            index,
                            id,
                            name,
                            elements,
                            outgoing_sequence_flows: vec![],
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
