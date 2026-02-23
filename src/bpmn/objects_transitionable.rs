use crate::bpmn::element::BPMNElement;

pub trait Transitionable {
    /// the number of transitions supported
    fn number_of_transitions(&self) -> usize;
}

impl Transitionable for Vec<BPMNElement> {
    fn number_of_transitions(&self) -> usize {
        self.iter().map(|x| x.number_of_transitions()).sum()
    }
}
