use ebi_activity_key::Activity;
use ebi_bpmn::ebi_arithmetic::Signed;
use intmap::IntKey;
use std::{
    fmt::Display,
    hash::Hash,
    ops::{Index, IndexMut},
};

use crate::{
    DeterministicFiniteAutomaton, DirectlyFollowsGraph, StochasticDeterministicFiniteAutomaton,
    StochasticNondeterministicFiniteAutomaton, ebi_objects::labelled_petri_net::TransitionIndex,
};

/// In an AutomatonSemantics, a state is an AutomatonState. A transition is a TransitionIndex,
/// which points uniquely to a target state and an activity. However, a transition may have multiple sources.
pub trait AutomatonSemantics {
    /// Returns the initial state of the execution semantics of the automaton.
    /// May return a virtual state.
    /// May return None, in which case the automaton has an empty language.
    fn initial_state(&self) -> Option<AutomatonState>;

    /// Returns the number of states in the execution semantics of the automaton.
    /// May be larger than the states -in- the automaton.
    fn number_of_states(&self) -> usize;

    /// Returns the states in the execution semantics of the automaton.
    /// May be larger than the states -in- the automaton.
    fn states(&self) -> impl Iterator<Item = AutomatonState>;

    /// Returns whether a state in the execution semantics of the automaton is a final state.
    fn is_state_final(&self, state: AutomatonState) -> bool;

    /// Returns the transitions in the execution semantics of the automaton.
    /// Some of these transitions may be virtual.
    /// Returns: (transition index, source state, target state, activity)
    /// Note that the indices of transitions are not unique.
    fn transitions(
        &self,
    ) -> impl Iterator<
        Item = (
            TransitionIndex,
            AutomatonState,
            AutomatonState,
            Option<Activity>,
        ),
    >;

    /// Returns the outgoing transitions of a state in the execution semantics of the automaton.
    /// Some of these transitions may be virtual.
    fn outgoing_transitions(&self, state: AutomatonState) -> Vec<TransitionIndex>;

    fn transition_2_target(&self, transition: TransitionIndex) -> Option<AutomatonState>;

    fn transition_2_activity(&self, transition: TransitionIndex) -> Option<Activity>;

    fn is_transition_silent(&self, transition: TransitionIndex) -> bool;
}

/**
 * States:
 * Initial state = nodes[len]
 * Final state = nodes[len] + 1. A final state is reached after executing a silent step from an end activity node.
 *
 * Transitions:
 * End activity/Initial state -> final state = edges[len]
 * initial state -> start activity = edges[len] + 1
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

    fn states(&self) -> impl Iterator<Item = AutomatonState> {
        (0..self.number_of_states()).map(|x| AutomatonState::of(x))
    }

    fn is_state_final(&self, state: AutomatonState) -> bool {
        state.0 > self.state_2_activity.len()
    }

    fn transitions(
        &self,
    ) -> impl Iterator<
        Item = (
            TransitionIndex,
            AutomatonState,
            AutomatonState,
            Option<Activity>,
        ),
    > {
        //model transitions
        let it1 = self
            .sources
            .iter()
            .zip(self.targets.iter().zip(self.weights.iter()))
            .enumerate()
            .filter(|(_, (_, (_, weight)))| weight.is_positive())
            .map(|(transition, (source, (target, _)))| {
                (
                    transition,
                    *source,
                    *target,
                    Some(self.state_2_activity[target]),
                )
            });

        //end transitions
        let end_transition = self.sources.len();
        let end_state = AutomatonState::of(self.state_2_activity.len() + 1);
        let it2 = self
            .end_activities
            .iter()
            .filter(|(_, weight)| weight.is_positive())
            .map(move |(state, _)| (end_transition, state, end_state, None));

        //start transitions
        let start_transition = self.sources.len() + 1;
        let start_state = AutomatonState::of(self.state_2_activity.len());
        let it3 = self
            .start_activities
            .iter()
            .filter(|(_, weight)| weight.is_positive())
            .map(move |(state, _)| {
                (
                    start_transition,
                    start_state,
                    state,
                    Some(self.state_2_activity[state]),
                )
            });

        it1.chain(it2).chain(it3)
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
        } else if transition < self.sources.len() + 1 + self.state_2_activity.len() {
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
        } else if transition < self.sources.len() + 1 + self.state_2_activity.len() {
            //start
            let node = transition - (self.sources.len() + 1);
            Some(self.state_2_activity[node])
        } else {
            None
        }
    }

    fn is_transition_silent(&self, transition: TransitionIndex) -> bool {
        transition == self.sources.len()
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

    fn states(&self) -> impl Iterator<Item = AutomatonState> {
        (0..self.number_of_states()).map(|x| AutomatonState::of(x))
    }

    fn is_state_final(&self, state: AutomatonState) -> bool {
        state.0 > self.final_states.len()
    }

    fn transitions(
        &self,
    ) -> impl Iterator<
        Item = (
            TransitionIndex,
            AutomatonState,
            AutomatonState,
            Option<Activity>,
        ),
    > {
        //model transitions
        let it1 = self
            .sources
            .iter()
            .zip(self.targets.iter().zip(self.activities.iter()))
            .enumerate()
            .map(|(transition, (source, (target, activity)))| {
                (transition, *source, *target, Some(*activity))
            });

        //end transitions
        let end_transition = self.sources.len();
        let end_state = AutomatonState::of(self.final_states.len());
        let it2 = self
            .final_states
            .iter()
            .enumerate()
            .filter(|(_, is_final)| **is_final)
            .map(move |(state, _)| (end_transition, AutomatonState::of(state), end_state, None));

        it1.chain(it2)
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

    fn is_transition_silent(&self, transition: TransitionIndex) -> bool {
        transition == self.sources.len()
    }
}

impl AutomatonSemantics for StochasticDeterministicFiniteAutomaton {
    fn initial_state(&self) -> Option<AutomatonState> {
        self.initial_state
    }

    fn number_of_states(&self) -> usize {
        self.terminating_probabilities.len() + 1
    }

    fn states(&self) -> impl Iterator<Item = AutomatonState> {
        (0..self.number_of_states()).map(|x| AutomatonState::of(x))
    }

    fn is_state_final(&self, state: AutomatonState) -> bool {
        state.0 > self.terminating_probabilities.len()
    }

    fn transitions(
        &self,
    ) -> impl Iterator<
        Item = (
            TransitionIndex,
            AutomatonState,
            AutomatonState,
            Option<Activity>,
        ),
    > {
        //model transitions
        let it1 = self
            .sources
            .iter()
            .zip(self.targets.iter().zip(self.activities.iter()))
            .enumerate()
            .map(|(transition, (source, (target, activity)))| {
                (transition, *source, *target, Some(*activity))
            });

        //end transitions
        let end_transition = self.sources.len();
        let end_state = AutomatonState::of(self.terminating_probabilities.len());
        let it2 = self
            .terminating_probabilities
            .iter()
            .enumerate()
            .filter(|(_, terminating_probability)| terminating_probability.is_positive())
            .map(move |(state, _)| (end_transition, AutomatonState::of(state), end_state, None));

        it1.chain(it2)
    }

    fn outgoing_transitions(&self, state: AutomatonState) -> Vec<TransitionIndex> {
        let mut result = vec![];

        //check the DFA for enabled transitions
        let (_, mut i) = self.binary_search(state, 0);
        while i < self.sources.len() && self.sources[i] == state {
            if self.probabilities[i].is_positive() {
                result.push(i);
            }
            i += 1;
        }

        //if the DFA can terminate, then add a termination silent transition
        if state.0 < self.terminating_probabilities.len()
            && self.terminating_probabilities[state.0].is_positive()
        {
            result.push(self.sources.len())
        }

        return result;
    }

    fn transition_2_target(&self, transition: TransitionIndex) -> Option<AutomatonState> {
        if transition == self.sources.len() {
            return Some(AutomatonState::of(self.terminating_probabilities.len()));
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

    fn is_transition_silent(&self, transition: TransitionIndex) -> bool {
        transition == self.sources.len()
    }
}

/**
 * terminating_probabilities.len() = final state
 *
 * 0..sources.len() = explicit transitions
 * sources.len()    = transition to final state
 */
impl AutomatonSemantics for StochasticNondeterministicFiniteAutomaton {
    fn initial_state(&self) -> Option<AutomatonState> {
        self.initial_state
    }

    fn number_of_states(&self) -> usize {
        self.sources.len() + 1
    }

    fn states(&self) -> impl Iterator<Item = AutomatonState> {
        (0..self.terminating_probabilities.len() + 1).map(|x| AutomatonState::of(x))
    }

    fn is_state_final(&self, state: AutomatonState) -> bool {
        state.0 == self.terminating_probabilities.len()
    }

    fn transitions(
        &self,
    ) -> impl Iterator<
        Item = (
            TransitionIndex,
            AutomatonState,
            AutomatonState,
            Option<Activity>,
        ),
    > {
        //model transitions
        let it1 = self
            .sources
            .iter()
            .zip(self.targets.iter().zip(self.activities.iter()))
            .enumerate()
            .map(|(transition, (source, (target, activity)))| {
                (transition, *source, *target, *activity)
            });

        //end transitions
        let end_transition = self.sources.len();
        let end_state = AutomatonState::of(self.terminating_probabilities.len());
        let it2 = self
            .terminating_probabilities
            .iter()
            .enumerate()
            .filter(|(_, terminating_probability)| terminating_probability.is_positive())
            .map(move |(state, _)| (end_transition, AutomatonState::of(state), end_state, None));

        it1.chain(it2)
    }

    fn outgoing_transitions(&self, state: AutomatonState) -> Vec<TransitionIndex> {
        let mut result = vec![];

        //check the DFA for enabled transitions
        let (_, mut i) = self.binary_search(state, 0, AutomatonState::zero());
        while i < self.sources.len() && self.sources[i] == state {
            if self.probabilities[i].is_positive() {
                result.push(i);
            }
            i += 1;
        }

        //if the DFA can terminate, then add a termination silent transition
        if state.0 <= self.terminating_probabilities.len()
            && self.terminating_probabilities[state].is_positive()
        {
            result.push(self.sources.len())
        }

        return result;
    }

    fn transition_2_target(&self, transition: TransitionIndex) -> Option<AutomatonState> {
        if transition >= self.sources.len() {
            return Some(AutomatonState::of(self.terminating_probabilities.len()));
        }

        Some(self.targets[transition])
    }

    fn transition_2_activity(&self, transition: TransitionIndex) -> Option<Activity> {
        if transition == self.sources.len() {
            None
        } else {
            self.activities[transition]
        }
    }

    fn is_transition_silent(&self, transition: TransitionIndex) -> bool {
        transition == self.sources.len()
    }
}

#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord)]
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

#[macro_export]
macro_rules! a {
    ($u:expr) => {
        crate::AutomatonState::of($u)
    };
}
pub use a;

#[cfg(test)]
mod tests {
    use crate::{AutomatonSemantics, DirectlyFollowsGraph, FiniteStochasticLanguage};
    use ebi_activity_key::{HasActivityKey, TranslateActivityKey};
    use std::fs;

    macro_rules! assert_execute_expect {
        ($tree:ident, $state:ident, $t:expr, $e:expr) => {
            println!("execute {} {}", ::std::stringify!($t), $t);
            assert!($tree.outgoing_transitions($state).contains(&$t));
            assert!(!$tree.is_state_final($state));
            $state = $tree.transition_2_target($t).unwrap();
            println!("state {}\n", $state);
            assert_eq!($tree.outgoing_transitions($state), $e);
        };
    }

    #[test]
    fn dfg_semantics() {
        let fin = fs::read_to_string("testfiles/bpic12-a.xes.gz-dfg.dfg").unwrap();
        let mut dfg = fin.parse::<DirectlyFollowsGraph>().unwrap();

        let fin2 = fs::read_to_string("testfiles/bpic12-a-sample.slang").unwrap();
        let mut slang = fin2.parse::<FiniteStochasticLanguage>().unwrap();

        dfg.translate_using_activity_key(slang.activity_key_mut());

        let a_submitted = dfg.activity_key_mut().process_activity("A_SUBMITTED");
        let a_partlysubmitted = dfg.activity_key_mut().process_activity("A_PARTLYSUBMITTED");
        let a_cancelled = dfg.activity_key_mut().process_activity("A_CANCELLED");

        println!("{:?}", dfg.activity_key);
        println!("node_2_activity  {:?}", dfg.state_2_activity);
        println!("start activities {:?}", dfg.start_activities);

        let mut state = dfg.initial_state().unwrap();
        println!("state {}\n", state);
        assert_eq!(dfg.outgoing_transitions(state), [16]);
        assert_eq!(dfg.transition_2_activity(16).unwrap(), a_submitted);

        assert_execute_expect!(dfg, state, 16, [0]);
        assert_eq!(dfg.transition_2_activity(0).unwrap(), a_partlysubmitted);

        assert_execute_expect!(dfg, state, 0, [1, 2, 3]);
        assert_eq!(dfg.transition_2_activity(3).unwrap(), a_cancelled);

        assert_execute_expect!(dfg, state, 3, [15]);

        assert_execute_expect!(dfg, state, 15, Vec::<usize>::new());
        assert!(dfg.is_state_final(state));
    }
}
