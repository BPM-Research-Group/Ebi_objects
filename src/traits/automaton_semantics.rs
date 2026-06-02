use crate::ebi_objects::labelled_petri_net::TransitionIndex;
use ebi_activity_key::Activity;
use intmap::IntKey;
use std::{
    fmt::Display,
    hash::Hash,
    ops::{Index, IndexMut},
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
    /// If the automaton is stochastic, then each returned transition has a non-zero weight.
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
