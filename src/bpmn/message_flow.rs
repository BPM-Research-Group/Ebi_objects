#[derive(Clone, Debug)]
pub struct MessageFlow {
    pub index: usize,
    pub id: String,
    pub source_pool_index: usize,
    pub source_element_index: usize,
    pub target_pool_index: usize,
    pub target_element_index: usize,
}
