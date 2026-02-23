pub(crate) static EMPTY_FLOWS: Vec<usize> = vec![];

pub trait BPMNObject {
    fn index(&self) -> usize;

    fn id(&self) -> &str;

    /// the flow indices of the incoming sequence flows of this object
    fn incoming_sequence_flows(&self) -> &[usize];

    /// the flow indices of the outgoing sequence flows of this object
    fn outgoing_sequence_flows(&self) -> &[usize];

    /// the flow indices of the incoming message flows of this object
    fn incoming_message_flows(&self) -> &[usize];

    /// the flow indices of the outgoing message flows of this object
    fn outgoing_message_flows(&self) -> &[usize];

    /// return whether this object could have incoming sequence flows according to the BPMN standard
    fn can_have_incoming_sequence_flows(&self) -> bool;
}
