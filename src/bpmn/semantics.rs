use crate::{
    Activity, BusinessProcessModelAndNotation,
    bpmn::{objects_objectable::BPMNObject, objects_transitionable::Transitionable},
};
use anyhow::Result;
use bitvec::vec::BitVec;

#[derive(Debug, PartialEq, Eq)]
pub struct SemState {
    pub(crate) marking_sequence_flows: Vec<u64>,
    marking_message_flows: Vec<u64>,
}
type TransitionIndex = usize;

impl BusinessProcessModelAndNotation {
    /// BPMN 2.0.2 standard page 238
    fn get_initial_state(&self) -> Option<SemState> {
        //find start events
        let start_events = self
            .all_elements_ref()
            .into_iter()
            .filter(|element| element.is_start_event())
            .collect::<Vec<_>>();

        let marking_message_flows = vec![0; self.number_of_message_flows()];

        //determine the initiation mode
        Some(if start_events.len() == 1 {
            //starting mode: through a start event
            let mut marking_sequence_flows = vec![0; self.number_of_sequence_flows()];
            for start_event in start_events {
                for sequence_flow in start_event.outgoing_sequence_flows() {
                    marking_sequence_flows[*sequence_flow] += 1;
                }
            }
            SemState {
                marking_sequence_flows,
                marking_message_flows,
            }
        } else if start_events.len() > 1 {
            //starting mode: choose any of multiple start events
            //add a place to the marking
            let mut marking_sequence_flows = vec![0; self.number_of_sequence_flows() + 1];
            marking_sequence_flows[self.number_of_sequence_flows()] = 1;
            SemState {
                marking_sequence_flows,
                marking_message_flows,
            }
        } else {
            //starting mode: (almost) every element without an incoming sequence flow is executed
            //add corresponding places to the marking
            todo!()
        })
    }

    fn execute_transition(&self, state: &mut SemState, transition: TransitionIndex) -> Result<()> {
        todo!()
    }

    fn is_final_state(&self, state: &SemState) -> bool {
        self.get_enabled_transitions(state).is_empty()
    }

    fn is_transition_silent(&self, transition: TransitionIndex) -> bool {
        todo!()
    }

    fn get_transition_activity(&self, transition: TransitionIndex) -> Option<Activity> {
        todo!()
    }

    fn get_enabled_transitions(&self, state: &SemState) -> Vec<TransitionIndex> {
        let mut result = BitVec::new();
        for element in self.all_elements_ref() {
            element.enabled_transitions(state, &mut result);
        }
        let mut result2 = Vec::new();
        for index in result.iter_ones() {
            result2.push(index);
        }
        result2
    }

    fn get_number_of_transitions(&self) -> usize {
        self.elements.number_of_transitions()
    }
}

#[cfg(test)]
mod tests {
    use crate::{BusinessProcessModelAndNotation, bpmn::semantics::SemState};
    use std::fs::{self};

    #[test]
    fn bpmn_semantics() {
        let fin = fs::read_to_string("testfiles/model.bpmn").unwrap();
        let bpmn = fin.parse::<BusinessProcessModelAndNotation>().unwrap();

        assert_eq!(
            bpmn.get_initial_state(),
            Some(SemState {
                marking_sequence_flows: vec![0, 0, 0, 0, 0, 1, 0, 0, 0, 0],
                marking_message_flows: vec![]
            })
        );
        assert_eq!(bpmn.get_number_of_transitions(), 12)
    }
}
