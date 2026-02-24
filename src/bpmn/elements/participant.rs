#[derive(Debug, Clone)]
///A white-box participant
pub struct BPMNParticipant {
    pub index: usize,
    pub id: String,
    pub name: Option<String>,
    pub process_id: String,
}
