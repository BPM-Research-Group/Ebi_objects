use crate::{
    Activity, BusinessProcessModelAndNotation,
    bpmn::{element::BPMNElement, objects::BPMNObject},
};
use anyhow::Result;

type SemState = Vec<u64>;
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

        //determine the initiation mode
        let result = if start_events.len() == 1 {
            //starting mode: through a start event
            let marking = vec![0; self.number_of_flows()];
            for start_event in start_events {
                for sequence_flow in start_event.outgoing_sequence_flows() {
                    marking[sequence_flow.index()] += 1;
                }
            }
            marking
        } else if start_events.len() > 1 {
            //starting mode: choose any of multiple start events
            //add a place to the marking
            let marking = vec![0; self.number_of_flows() + 1];
            marking[self.number_of_flows()] = 1;
            marking
        } else {
            //starting mode: (almost) every element without an incoming sequence flow is executed
            //add corresponding places to the marking
            todo!()
        };

        Some(result)
    }

    fn execute_transition(&self, state: &mut SemState, transition: TransitionIndex) -> Result<()> {
        todo!()
    }

    fn is_final_state(&self, state: &SemState) -> bool {
        todo!()
    }

    fn is_transition_silent(&self, transition: TransitionIndex) -> bool {
        todo!()
    }

    fn get_transition_activity(&self, transition: TransitionIndex) -> Option<Activity> {
        todo!()
    }

    fn get_enabled_transitions(&self, state: &SemState) -> Vec<TransitionIndex> {
        todo!()
    }

    fn get_number_of_transitions(&self) -> usize {
        todo!()
    }
}
