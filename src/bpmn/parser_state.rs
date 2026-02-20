use crate::{
    ActivityKey, BusinessProcessModelAndNotation,
    bpmn::{importer::parse_attribute, tag_definitions::DraftDefinitions, tags::OpenedTag},
};
use anyhow::{Context, Result, anyhow};
use quick_xml::events::BytesStart;
use std::collections::{HashMap, hash_map::Entry};

pub(crate) struct ParserState {
    pub(crate) activity_key: ActivityKey,
    pub(crate) open_tag_names: Vec<Vec<u8>>,
    pub(crate) open_tags: Vec<OpenedTag>,
    pub(crate) ids: HashMap<String, usize>,

    pub(crate) draft_definitionss: Vec<DraftDefinitions>,
}

impl ParserState {
    pub(crate) fn new() -> Self {
        Self {
            activity_key: ActivityKey::new(),
            open_tag_names: vec![],
            ids: HashMap::new(),
            open_tags: vec![],
            draft_definitionss: vec![],
        }
    }

    pub(crate) fn to_model(self) -> Result<BusinessProcessModelAndNotation> {
        let ParserState {
            activity_key,
            mut draft_definitionss,
            ..
        } = self;
        if draft_definitionss.len() == 1 {
            let draft_definition = draft_definitionss.remove(0);
            let DraftDefinitions {
                index,
                id,
                collaboration_index,
                collaboration_id,
                processes,
                message_flows,
            } = draft_definition;
            //construct result
            let result = BusinessProcessModelAndNotation {
                activity_key,
                collaboration_index,
                collaboration_id,
                definitions_index: index,
                definitions_id: id,
                processes: processes,
                message_flows,
            };

            //check structural correctness
            result
                .is_structurally_correct()
                .with_context(|| "model is not structurally correct")?;
            Ok(result)
        } else {
            if draft_definitionss.len() == 0 {
                Err(anyhow!("no process found"))
            } else {
                Err(anyhow!("multiple processes found"))
            }
        }
    }

    pub(crate) fn read_and_add_id(&mut self, e: &BytesStart) -> Result<(usize, String)> {
        let new_index = self.ids.len();
        if let Some(id) = parse_attribute(e, "id") {
            match self.ids.entry(id.clone()) {
                Entry::Occupied(_) => Err(anyhow!("two elements have the id `{}`", id)),
                Entry::Vacant(vacant_entry) => {
                    vacant_entry.insert(new_index);
                    Ok((new_index, id))
                }
            }
        } else {
            Err(anyhow!("element must have an id"))
        }
    }
}
