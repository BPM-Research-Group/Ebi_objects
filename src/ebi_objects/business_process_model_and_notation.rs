#[cfg(any(test, feature = "testactivities"))]
use crate::activity_key::has_activity_key::TestActivityKey;
use crate::{Activity, ActivityKey, Graphable, Infoable, TranslateActivityKey};
use anyhow::Result;
use ebi_derive::ActivityKey;
use std::{collections::HashMap, fmt::Display};

#[derive(Clone, ActivityKey)]
pub struct BusinessProcessModelAndNotation {
    pub(crate) activity_key: ActivityKey,

    /// pools
    pub processes: Vec<BPMNProcess>,

    /// messages
    pub message_flows: Vec<MessageFlow>,
}

#[derive(Clone)]
pub struct BPMNProcess {
    pub element_id_2_index: HashMap<String, usize>,
    pub element_index_2_element: Vec<BPMNElement>,
    pub flow_id_2_index: HashMap<String, usize>,
    pub flow_index_2_flow: Vec<SequenceFlow>,
}

#[derive(Clone, Debug)]
pub enum BPMNElement {
    StartEvent,
    EndEvent,
    Task { activity: Activity },
    ExclusiveGateway,
    InclusiveGateway,
}

#[derive(Clone)]
pub struct SequenceFlow {
    pub(crate) source_element_index: usize,
    pub(crate) target_element_index: usize,
}

#[derive(Clone)]
pub struct MessageFlow {
    pub(crate) id: String,
    pub(crate) source_pool: usize,
    pub(crate) source_element: usize,
    pub(crate) target_pool: usize,
    pub(crate) target_element: usize,
}

impl Display for BusinessProcessModelAndNotation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

impl Infoable for BusinessProcessModelAndNotation {
    fn info(&self, f: &mut impl std::io::Write) -> Result<()> {
        todo!()
    }
}

impl Graphable for BusinessProcessModelAndNotation {
    fn to_dot(&self) -> Result<layout::topo::layout::VisualGraph> {
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
