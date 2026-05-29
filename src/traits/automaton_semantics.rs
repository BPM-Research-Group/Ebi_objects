use ebi_activity_key::Activity;
use ebi_bpmn::ebi_arithmetic::Signed;
use intmap::IntKey;
use resvg::usvg::AlignmentBaseline::Auto;
use std::{
    fmt::Display,
    hash::Hash,
    ops::{Index, IndexMut},
};

use crate::{
    DeterministicFiniteAutomaton, DirectlyFollowsGraph,
    ebi_objects::labelled_petri_net::TransitionIndex,
};

pub trait AutomatonSemantics {
    fn initial_state(&self) -> Option<AutomatonState>;

    fn number_of_states(&self) -> usize;

    fn number_of_transitions(&self) -> usize;

    fn states(&self) -> impl Iterator<Item = AutomatonState>;

    fn is_state_final(&self, state: AutomatonState) -> bool;

    fn transitions(&self) -> impl Iterator<Item = TransitionIndex>;

    fn outgoing_transitions(&self, state: AutomatonState) -> Vec<TransitionIndex>;

    fn transition_2_target(&self, transition: TransitionIndex) -> Option<AutomatonState>;

    fn transition_2_activity(&self, transition: TransitionIndex) -> Option<Activity>;
}

/**
 * States:
 * Initial state = nodes[len]
 * Final state = nodes[len] + 1. A final state is reached after executing a silent step from an end activity node.
 *
 * Transitions:
 * End activity/Initial state -> final state = edges[len]
 * initial state -> start activity = edges[len] + 1 + ...
 */
impl AutomatonSemantics for DirectlyFollowsGraph {
    fn initial_state(&self) -> Option<AutomatonState> {
        if !self.has_empty_traces() && !self.start_activities.iter().any(|(_, w)| w.is_positive()) {
            None
        } else {
            Some(AutomatonState::of(self.state_2_activity.len()))
        }
    }

    fn number_of_states(&self) -> usize {
        self.state_2_activity.len() + 2
    }

    fn number_of_transitions(&self) -> usize {
        self.sources.len() + 1 + self.state_2_activity.len()
    }

    fn states(&self) -> impl Iterator<Item = AutomatonState> {
        (0..self.number_of_states()).map(|x| AutomatonState::of(x))
    }

    fn is_state_final(&self, state: AutomatonState) -> bool {
        state.0 > self.state_2_activity.len()
    }

    fn transitions(&self) -> impl Iterator<Item = TransitionIndex> {
        0..self.number_of_transitions()
    }

    fn outgoing_transitions(&self, state: AutomatonState) -> Vec<usize> {
        if state.0 == self.state_2_activity.len() {
            //we are in the initial state
            let mut result = self
                .start_activities
                .iter()
                .filter_map(|(node, w)| {
                    if w.is_positive() {
                        Some(self.sources.len() + 1 + node.0)
                    } else {
                        None
                    }
                })
                .collect::<Vec<_>>();

            if self.empty_traces_weight.is_positive() {
                result.push(self.sources.len())
            }

            result
        } else if state.0 > self.state_2_activity.len() {
            //we are in the final state
            vec![]
        } else {
            //we are not in the initial state
            let mut result = vec![];

            //add edges
            let (_, mut i) = self.binary_search(state, AutomatonState::zero());
            while i < self.sources.len() && self.sources[i] == state {
                if self.weights[i].is_positive() {
                    result.push(i);
                }
                i += 1;
            }

            //add transition to final state
            if self.is_end_node(self.state_2_activity[state]) {
                result.push(self.sources.len())
            }

            result
        }
    }

    fn transition_2_target(&self, transition: TransitionIndex) -> Option<AutomatonState> {
        if transition == self.sources.len() {
            //end / empty
            Some(AutomatonState::of(self.state_2_activity.len() + 1))
        } else if transition < self.sources.len() {
            //edge
            Some(AutomatonState::of(self.targets[transition].0))
        } else if transition < self.number_of_transitions() {
            //start
            Some(AutomatonState::of(transition - (self.sources.len() + 1)))
        } else {
            None
        }
    }

    fn transition_2_activity(&self, transition: TransitionIndex) -> Option<Activity> {
        if transition == self.sources.len() {
            //end
            None
        } else if transition < self.sources.len() {
            //edge
            let node = transition;
            Some(self.state_2_activity[self.targets[node].0])
        } else if transition < self.number_of_transitions() {
            //start
            let node = transition - (self.sources.len() + 1);
            Some(self.state_2_activity[node])
        } else {
            None
        }
    }
}

/**
 * states.len() = final state
 * sources.len() = silent transition to final state
 */
impl AutomatonSemantics for DeterministicFiniteAutomaton {
    fn initial_state(&self) -> Option<AutomatonState> {
        self.initial_state
    }

    fn number_of_states(&self) -> usize {
        self.final_states.len() + 1
    }

    fn number_of_transitions(&self) -> usize {
        self.sources.len() + 1
    }

    fn states(&self) -> impl Iterator<Item = AutomatonState> {
        (0..self.number_of_states()).map(|x| AutomatonState::of(x))
    }

    fn is_state_final(&self, state: AutomatonState) -> bool {
        state.0 > self.final_states.len()
    }

    fn transitions(&self) -> impl Iterator<Item = TransitionIndex> {
        0..(self.sources.len() + 1)
    }

    fn outgoing_transitions(&self, state: AutomatonState) -> Vec<TransitionIndex> {
        let mut result = vec![];

        //check the DFA for enabled transitions
        let (_, mut i) = self.binary_search(state, 0);
        while i < self.sources.len() && self.sources[i] == state {
            result.push(i);
            i += 1;
        }

        //if the DFA can terminate, then add a termination silent transition
        if state.0 < self.final_states.len() && self.final_states[state.0] {
            result.push(self.sources.len())
        }

        return result;
    }

    fn transition_2_target(&self, transition: TransitionIndex) -> Option<AutomatonState> {
        if transition == self.sources.len() {
            return Some(AutomatonState::of(self.final_states.len()));
        }

        Some(self.targets[transition])
    }

    fn transition_2_activity(&self, transition: TransitionIndex) -> Option<Activity> {
        if transition == self.sources.len() {
            None
        } else {
            Some(self.activities[transition])
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq)]
pub struct AutomatonState(pub usize);

impl AutomatonState {
    pub fn zero() -> Self {
        AutomatonState(0)
    }

    pub fn of(id: usize) -> Self {
        AutomatonState(id)
    }

    pub fn of_option(id: Option<usize>) -> Option<Self> {
        Some(AutomatonState::of(id?))
    }
}

impl Display for AutomatonState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl Hash for AutomatonState {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

impl IntKey for AutomatonState {
    type Int = usize;

    const PRIME: Self::Int = (u32::MAX - 4) as usize;

    fn into_int(self) -> Self::Int {
        self.0
    }
}

impl<T> Index<AutomatonState> for Vec<T> {
    type Output = T;

    fn index(&self, index: AutomatonState) -> &Self::Output {
        &self[index.0]
    }
}

impl<T> Index<&AutomatonState> for Vec<T> {
    type Output = T;

    fn index(&self, index: &AutomatonState) -> &Self::Output {
        &self[index.0]
    }
}

impl<T> IndexMut<AutomatonState> for Vec<T> {
    fn index_mut(&mut self, index: AutomatonState) -> &mut T {
        &mut self[index.0]
    }
}

impl<T> IndexMut<&AutomatonState> for Vec<T> {
    fn index_mut(&mut self, index: &AutomatonState) -> &mut T {
        &mut self[index.0]
    }
}
