#[cfg(any(test, feature = "testactivities"))]
use crate::activity_key::has_activity_key::TestActivityKey;
use crate::{
    ActivityKey, Graphable, Infoable, TranslateActivityKey,
    bpmn::objects::{BPMNProcess, MessageFlow},
};
use anyhow::Result;
use ebi_derive::ActivityKey;
use layout::topo::layout::VisualGraph;
use std::{
    fmt::{Display, Formatter},
    io::Write,
};

#[derive(Clone, ActivityKey)]
pub struct BusinessProcessModelAndNotation {
    pub(crate) activity_key: ActivityKey,

    pub collaboration_index: Option<usize>,
    pub collaboration_id: Option<String>,
    pub definitions_index: usize,
    pub definitions_id: String,

    /// pools
    pub processes: Vec<BPMNProcess>,

    /// messages
    pub message_flows: Vec<MessageFlow>,
}

impl Display for BusinessProcessModelAndNotation {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

impl Infoable for BusinessProcessModelAndNotation {
    fn info(&self, f: &mut impl Write) -> Result<()> {
        todo!()
    }
}

impl Graphable for BusinessProcessModelAndNotation {
    fn to_dot(&self) -> Result<VisualGraph> {
        todo!()
    }
}

impl TranslateActivityKey for BusinessProcessModelAndNotation {
    fn translate_using_activity_key(&mut self, to_activity_key: &mut ActivityKey) {
        todo!()
    }
}

#[cfg(any(test, feature = "testactivities"))]
impl TestActivityKey for BusinessProcessModelAndNotation {
    fn test_activity_key(&self) {
        todo!()
    }
}
