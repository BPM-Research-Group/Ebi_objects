#[derive(Clone, Debug)]
pub struct BPMNSequenceFlow {
    pub index: usize,
    pub id: String,
    pub flow_index: usize,
    pub source_index: usize,
    pub target_index: usize,
}
