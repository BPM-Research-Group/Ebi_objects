#[derive(Clone, Debug)]
pub struct SequenceFlow {
    pub index: usize,
    pub id: String,
    pub flow_index: usize,
    pub source_index: usize,
    pub target_index: usize,
}
