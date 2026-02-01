use crate::{
    Activity, ActivityKey, ActivityKeyTranslator, EbiObject, Exportable, Graphable, HasActivityKey,
    Importable, Infoable, TranslateActivityKey, dfg_format_comparison,
    line_reader::LineReader,
    traits::{
        graphable,
        importable::{ImporterParameter, ImporterParameterValues, from_string},
    },
};
use anyhow::{Context, Result, anyhow};
use ebi_arithmetic::{Fraction, One, Signed, Zero};
use ebi_derive::ActivityKey;
use layout::topo::layout::VisualGraph;
use std::{
    cmp::{Ordering, max},
    collections::HashMap,
    fmt::Display,
};

pub const HEADER: &str = "stochastic non-deterministic finite automaton";

#[derive(Clone, Debug, ActivityKey)]
pub struct StochasticNondeterministicFiniteAutomaton {
    pub activity_key: ActivityKey,

    /// If the initial state is None, then the language is empty
    pub initial_state: Option<usize>,

    pub sources: Vec<usize>,               //transition -> source of arc
    pub targets: Vec<usize>,               //transition -> target of arc
    pub activities: Vec<Option<Activity>>, //transition -> activity of arc
    pub probabilities: Vec<Fraction>,      //transition -> probability of arc
    pub terminating_probabilities: Vec<Fraction>, //state -> termination probability
}

impl StochasticNondeterministicFiniteAutomaton {
    /// Create an empty SNFA with a single initial / final state
    pub fn new() -> Self {
        Self {
            activity_key: ActivityKey::new(),
            initial_state: Some(0),
            sources: vec![],
            targets: vec![],
            activities: vec![],
            probabilities: vec![],
            terminating_probabilities: vec![Fraction::one()],
        }
    }

    pub fn get_sources(&self) -> &Vec<usize> {
        &self.sources
    }

    pub fn get_targets(&self) -> &Vec<usize> {
        &self.targets
    }

    pub fn get_activities(&self) -> &Vec<Option<Activity>> {
        &self.activities
    }

    pub fn get_probabilities(&self) -> &Vec<Fraction> {
        &self.probabilities
    }

    pub fn set_initial_state(&mut self, state: Option<usize>) {
        if let Some(state) = state {
            self.ensure_states(state);
        }
        self.initial_state = state;
    }

    pub fn can_terminate_in_state(&self, state: usize) -> bool {
        self.get_termination_probability(state).is_positive()
    }

    /**
     * Returns whether a transition is not permanently disabled.
     */
    pub fn can_execute_transition(&self, transition: usize) -> bool {
        self.probabilities[transition].is_positive()
    }

    /// Ensures that a state with index `new_max_state` exists
    fn ensure_states(&mut self, new_max_state: usize) {
        while self.number_of_states() <= new_max_state {
            self.add_state();
        }
    }

    /// Add a transition. Will reduce the termination probability of the source state accordingly.
    pub fn add_transition(
        &mut self,
        source: usize,
        activity: Option<Activity>,
        target: usize,
        probability: Fraction,
    ) -> Result<()> {
        self.ensure_states(max(source, target));

        self.terminating_probabilities[source] -= &probability;

        let (found, from) = self.binary_search(source, self.label_to_id(activity), target);
        if found {
            //edge already present
            self.probabilities[from] += probability;
        } else {
            self.sources.insert(from, source);
            self.targets.insert(from, target);
            self.activities.insert(from, activity);
            self.probabilities.insert(from, probability);

            if self.terminating_probabilities[source].is_negative() {
                return Err(anyhow!(
                    "tried to insert edge from {} to {}, which brings the sum outgoing probability of the source state (1-{}) above 1",
                    source,
                    target,
                    self.terminating_probabilities[source]
                ));
            }
        }
        Ok(())
    }

    pub fn number_of_transitions(&self) -> usize {
        self.sources.len()
    }

    /**
     * Adds the probability to the transition. Returns the target state, which may be new.
     * The caller is responsible for updating the termination probability.
     * This function will not introduce non-determinism.
     */
    pub fn take_or_add_transition(
        &mut self,
        source_state: usize,
        activity: Option<Activity>,
        probability: Fraction,
    ) -> usize {
        self.terminating_probabilities[source_state] -= &probability;

        let (found, mut transition) =
            self.binary_search(source_state, self.label_to_id(activity), 0);
        if found
            || (transition < self.sources.len()
                && self.sources[transition] == source_state
                && self.activities[transition] == activity)
        {
            //there is already an edge (source, activity, target); add to its probability
            self.probabilities[transition] += &probability;
            return self.targets[transition];
        } else {
            //find the last position where our edge could be
            let (_, transition2) = self.binary_search(
                source_state,
                self.label_to_id(activity),
                self.number_of_states(),
            );

            //our edge could be anywhere between transition1 (inclusive) and transition2 (inclusive)
            if transition != transition2 {
                // There is already at least one transition of (source, activity, _).
                // Add the probability to that transition.
                transition += 1;
                self.probabilities[transition] += &probability;
                return self.targets[transition];
            } else {
                let target = self.add_state();
                self.sources.insert(transition, source_state);
                self.targets.insert(transition, target);
                self.activities.insert(transition, activity);
                self.probabilities.insert(transition, probability);
                return target;
            }
        }
    }

    /**
     * Scales the outgoing probabilities of the state.
     */
    pub fn scale_outgoing_probabilities(&mut self, state2scale: HashMap<usize, Fraction>) {
        let mut new_terminating_probabilities =
            vec![Fraction::one(); self.terminating_probabilities.len()];
        for (state, _, _, outgoing_probability) in &mut self.into_iter() {
            if let Some(factor) = state2scale.get(state) {
                *outgoing_probability /= factor;
            }
            new_terminating_probabilities[*state] -= outgoing_probability;
        }
        self.terminating_probabilities = new_terminating_probabilities;
    }

    /// Returns the number of states.
    pub fn number_of_states(&self) -> usize {
        self.terminating_probabilities.len()
    }

    pub fn get_initial_state(&self) -> Option<usize> {
        self.initial_state
    }

    pub fn get_termination_probability(&self, state: usize) -> &Fraction {
        &self.terminating_probabilities[state]
    }

    //Adds a state with 1 terminating probability.
    pub fn add_state(&mut self) -> usize {
        let state = self.terminating_probabilities.len();
        self.terminating_probabilities.push(Fraction::one());
        return state;
    }

    fn compare(
        source1: usize,
        activity1: usize,
        target1: usize,
        source2: usize,
        activity2: usize,
        target2: usize,
    ) -> Ordering {
        if source1 < source2 {
            return Ordering::Greater;
        } else if source1 > source2 {
            return Ordering::Less;
        } else if activity2 > activity1 {
            return Ordering::Greater;
        } else if activity2 < activity1 {
            return Ordering::Less;
        } else if target1 < target2 {
            return Ordering::Greater;
        } else if target1 > target2 {
            return Ordering::Less;
        } else {
            return Ordering::Equal;
        }
    }

    pub fn label_to_id(&self, label: Option<Activity>) -> usize {
        if let Some(activity) = label {
            1 + activity.id
        } else {
            0
        }
    }

    pub fn binary_search(&self, source: usize, activity: usize, target: usize) -> (bool, usize) {
        if self.sources.is_empty() {
            return (false, 0);
        }

        let mut size = self.sources.len();
        let mut left = 0;
        let mut right = size;
        while left < right {
            let mid = left + size / 2;

            let cmp = Self::compare(
                source,
                activity,
                target,
                self.sources[mid],
                self.label_to_id(self.activities[mid]),
                self.targets[mid],
            );

            left = if cmp == Ordering::Less { mid + 1 } else { left };
            right = if cmp == Ordering::Greater { mid } else { right };
            if cmp == Ordering::Equal {
                assert!(mid < self.sources.len());
                return (true, mid);
            }

            size = right - left;
        }

        assert!(left <= self.sources.len());
        (false, left)
    }

    pub fn check_consistency(&self) -> Result<()> {
        let zero = Fraction::zero();
        //verify that sum of outgoing edges for each state is one
        if self.number_of_states() > 0 {
            let mut state = 0; //next state to check
            let mut transition = 0; //next transition to check
            let mut sum = self.terminating_probabilities[0].clone(); //sum probability of the next state
            while state < self.number_of_states() {
                if transition == self.number_of_transitions() {
                    //there are no more transitions, so move to the next state
                    if (sum > Fraction::one() || sum < Fraction::one()) && !sum.is_one() {
                        return Err(anyhow!(
                            "outgoing probabilities of state {} do not sum to 1, but to {}",
                            state,
                            self.terminating_probabilities[state]
                        ));
                    }
                    state += 1;
                    sum = self
                        .terminating_probabilities
                        .get(state)
                        .unwrap_or(&zero)
                        .clone();
                } else if state == self.sources[transition] {
                    //the next transition is of the same state; add its probability
                    sum += &self.probabilities[transition];
                    transition += 1;
                } else {
                    //the next transition is of a different state; finish the current state and move to a new one
                    if (sum > Fraction::one() || sum < Fraction::one()) && !sum.is_one() {
                        return Err(anyhow!(
                            "outgoing probabilities of state {} do not sum to 1, but to {}",
                            state,
                            self.terminating_probabilities[state]
                        ));
                    }
                    state += 1;
                    sum = self
                        .terminating_probabilities
                        .get(state)
                        .unwrap_or(&zero)
                        .clone();
                }
            }
        }
        Ok(())
    }

    /// Returns an iterator over the outgoing edges of `source`.
    /// That is, (source, target, label, probaiblity)
    pub fn outgoing_edges(
        &'_ self,
        source: usize,
    ) -> StochasticNondeterministicFiniteAutomatonIterator<'_> {
        let (_, start) = self.binary_search(source, 0, 0);
        let (_, end) = self.binary_search(source, usize::MAX, usize::MAX);
        StochasticNondeterministicFiniteAutomatonIterator {
            it_sources: self.get_sources()[start..end].iter(),
            it_targets: self.get_targets()[start..end].iter(),
            it_activities: self.get_activities()[start..end].iter(),
            it_probabilities: self.get_probabilities()[start..end].iter(),
        }
    }
}

impl TranslateActivityKey for StochasticNondeterministicFiniteAutomaton {
    fn translate_using_activity_key(&mut self, to_activity_key: &mut ActivityKey) {
        let translator = ActivityKeyTranslator::new(&self.activity_key, to_activity_key);

        //as the order of activities may have changed, we need to rebuild the transitions
        let mut sources = Vec::with_capacity(self.number_of_transitions());
        let mut targets = Vec::with_capacity(self.number_of_transitions());
        let mut probabilities = Vec::with_capacity(self.number_of_transitions());
        let mut activities = Vec::with_capacity(self.number_of_transitions());

        std::mem::swap(&mut self.sources, &mut sources);
        std::mem::swap(&mut self.targets, &mut targets);
        std::mem::swap(&mut self.probabilities, &mut probabilities);
        std::mem::swap(&mut self.activities, &mut activities);

        /*
         * The order will remain largely the same, as transitions are first sorted by source, which does not change.
         * Therefore, we add the transitions back in the original order as much as possible.
         * A potential further optimisation would be to apply a proper sorting algorithm, or to avoid binary search.
         */
        for (source, (target, (probability, activity))) in sources.into_iter().zip(
            targets
                .into_iter()
                .zip(probabilities.into_iter().zip(activities.into_iter())),
        ) {
            let activity = if let Some(activity) = activity {
                Some(translator.translate_activity(&activity))
            } else {
                None
            };
            let (_, from) = self.binary_search(source, self.label_to_id(activity), target);

            self.sources.insert(from, source);
            self.targets.insert(from, target);
            self.activities.insert(from, activity);
            self.probabilities.insert(from, probability);
        }
        self.activity_key = to_activity_key.clone();
    }
}

impl Display for StochasticNondeterministicFiniteAutomaton {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}", HEADER)?;
        if let Some(i) = self.initial_state {
            writeln!(f, "# initial state\n{:?}", i)?;
        } else {
            writeln!(f, "#initial state\nnone")?;
        }
        writeln!(f, "# number of states\n{}", self.number_of_states())?;
        writeln!(
            f,
            "# number of transitions\n{}",
            self.number_of_transitions()
        )?;

        for transition in 0..self.number_of_transitions() {
            writeln!(
                f,
                "# transition {}\n# source\n{}\n# target\n{}\n# probability\n{}",
                transition,
                self.sources[transition],
                self.targets[transition],
                self.probabilities[transition]
            )?;
            if let Some(act) = self.activities[transition] {
                writeln!(
                    f,
                    "# activity\nlabel {}",
                    self.activity_key.get_activity_label(&act)
                )?;
            } else {
                writeln!(f, "# activity\nsilent")?;
            }
        }
        Ok(())
    }
}

impl Importable for StochasticNondeterministicFiniteAutomaton {
    const IMPORTER_PARAMETERS: &[ImporterParameter] = &[];

    const FILE_FORMAT_SPECIFICATION_LATEX: &str = concat!("A stochastic non-deterministic finite automaton is a line-based structure. Lines starting with a \\# are ignored.
    This first line is exactly `stochastic non-deterministic finite automaton'.
    The second line is the initial state of the SNFA, or the word `None'.
    The third line is the number of states in the SNFA.
    The fourth line is the number of transitions in the SNFA.
    Then, for each transition, the following lines are next: 
    (i) the source state of the transition;
    (ii) the target state of the transition;
    (iii) the probability of the transition;
    (iv) the word `silent' or the word `label' followed by a space and the name of the activity with which the transition is labelled.

    A stochastic non-deterministic finite automaton will have one probability for each (source, activity, target).
    
    For instance:
    \\lstinputlisting[language=ebilines, style=boxed]{../testfiles/aa-ab-ba.snfa}", dfg_format_comparison!());

    fn import_as_object(
        reader: &mut dyn std::io::BufRead,
        parameter_values: &ImporterParameterValues,
    ) -> Result<EbiObject> {
        Ok(EbiObject::StochasticNondeterministicFiniteAutomaton(
            Self::import(reader, parameter_values)?,
        ))
    }

    fn import(
        reader: &mut dyn std::io::BufRead,
        _parameter_values: &ImporterParameterValues,
    ) -> Result<Self>
    where
        Self: Sized,
    {
        let mut lreader = LineReader::new(reader);

        let mut result = Self::new();

        let head = lreader
            .next_line_string()
            .with_context(|| format!("failed to read header, which should be {}", HEADER))?;
        if head != HEADER {
            return Err(anyhow!(
                "first line should be exactly `{}`, but found `{}` on line `{}`",
                HEADER,
                lreader.get_last_line(),
                lreader.get_last_line_number()
            ));
        }

        //read initial state
        let initial_state = lreader
            .next_line_string()
            .with_context(|| "failed to read initial state")?;
        result.initial_state = if &initial_state.trim_start().to_lowercase()[..] == "none" {
            None
        } else {
            Some(
                initial_state
                    .parse::<usize>()
                    .with_context(|| "failed to read initial state")?,
            )
        };

        //read the number of states
        let number_of_states = lreader
            .next_line_index()
            .with_context(|| "failed to read number of states")?;
        for _ in 1..number_of_states {
            result.add_state();
        }

        //read the number of transitions
        let number_of_transitions = lreader
            .next_line_index()
            .with_context(|| "failed to read number of transitions")?;

        for transition in 0..number_of_transitions {
            //read transitions

            //read source
            let source = lreader.next_line_index().with_context(|| {
                format!("could not read source state of transition {}", transition)
            })?;
            if source >= result.number_of_states() {
                return Err(anyhow!(
                    "transition {} has an invalid source {}",
                    transition,
                    source
                ));
            }

            //read target
            let target = lreader.next_line_index().with_context(|| {
                format!("could not read target state of transition {}", transition)
            })?;
            if target >= result.number_of_states() {
                return Err(anyhow!(
                    "transition {} has an invalid target {}",
                    transition,
                    target
                ));
            }

            //read probability
            let probability = lreader.next_line_weight().with_context(|| {
                format!("could not read probability of transition {}", transition)
            })?;
            if !probability.is_positive() {
                return Err(anyhow!(
                    "probability of transition {} is not positive: {}",
                    transition,
                    probability
                ));
            }

            //read label
            let label_line = lreader
                .next_line_string()
                .with_context(|| format!("failed to read label of transition {}", transition))?;
            let label = if label_line.trim_start().starts_with("label ") {
                let label = label_line.trim_start()[6..].to_string();
                let activity = result.activity_key.process_activity(&label);
                Some(activity)
            } else if label_line.trim_start().starts_with("silent") {
                None
            } else {
                return Err(anyhow!(
                    "could not read label of transition {}: expected 'silent' or 'label ', but found '{}'",
                    transition,
                    label_line
                ));
            };

            result
                .add_transition(source, label, target, probability)
                .with_context(|| format!("could not add transition {}", transition))?;
        }

        result.check_consistency()?;

        Ok(result)
    }
}
from_string!(StochasticNondeterministicFiniteAutomaton);

impl Infoable for StochasticNondeterministicFiniteAutomaton {
    fn info(&self, f: &mut impl std::io::Write) -> Result<()> {
        writeln!(f, "Number of states\t{}", self.number_of_states())?;
        writeln!(f, "Number of transitions\t{}", self.number_of_transitions())?;
        writeln!(
            f,
            "Number of activities\t{}",
            self.activity_key.get_number_of_activities()
        )?;

        writeln!(f, "")?;
        self.activity_key().info(f)?;

        Ok(writeln!(f, "")?)
    }
}

impl Graphable for StochasticNondeterministicFiniteAutomaton {
    fn to_dot(&self) -> Result<layout::topo::layout::VisualGraph> {
        let mut graph = VisualGraph::new(layout::core::base::Orientation::LeftToRight);

        let mut places = vec![];
        for termination_probability in &self.terminating_probabilities {
            places.push(graphable::create_place(
                &mut graph,
                &format!("{}", termination_probability),
            ));
        }

        for transition in 0..self.number_of_transitions() {
            let source = places[self.sources[transition]];
            let target = places[self.targets[transition]];
            let probability = &self.probabilities[transition];
            let activity = if let Some(activity) = self.activities[transition] {
                self.activity_key.get_activity_label(&activity)
            } else {
                "τ"
            };

            graphable::create_edge(
                &mut graph,
                &source,
                &target,
                &format!("{}, {}", activity, probability.to_string()),
            );
        }

        Ok(graph)
    }
}

impl Exportable for StochasticNondeterministicFiniteAutomaton {
    fn export_from_object(object: crate::EbiObject, f: &mut dyn std::io::Write) -> Result<()> {
        match object {
            EbiObject::StochasticNondeterministicFiniteAutomaton(sdfa) => sdfa.export(f),
            EbiObject::StochasticDeterministicFiniteAutomaton(sdfa) => {
                Into::<Self>::into(sdfa).export(f)
            }
            EbiObject::FiniteStochasticLanguage(slang) => Into::<Self>::into(slang).export(f),
            EbiObject::StochasticProcessTree(sptree) => Into::<Self>::into(sptree).export(f),
            EbiObject::EventLog(log) => Into::<Self>::into(log).export(f),
            EbiObject::EventLogTraceAttributes(log) => Into::<Self>::into(log).export(f),
            EbiObject::EventLogXes(log) => Into::<Self>::into(log).export(f),
            EbiObject::EventLogCsv(log) => Into::<Self>::into(log).export(f),
            EbiObject::StochasticDirectlyFollowsModel(slang) => Into::<Self>::into(slang).export(f),
            _ => Err(anyhow!(
                "Cannot export {} {} as an SNFA.",
                object.get_type().get_article(),
                object.get_type()
            )),
        }
    }

    fn export(&self, f: &mut dyn std::io::Write) -> Result<()> {
        Ok(write!(f, "{}", self)?)
    }
}

impl<'a> IntoIterator for &'a StochasticNondeterministicFiniteAutomaton {
    type Item = (&'a usize, &'a usize, &'a Option<Activity>, &'a Fraction);

    type IntoIter = StochasticNondeterministicFiniteAutomatonIterator<'a>;

    fn into_iter(self) -> Self::IntoIter {
        Self::IntoIter {
            it_sources: self.get_sources().iter(),
            it_targets: self.get_targets().iter(),
            it_activities: self.get_activities().iter(),
            it_probabilities: self.get_probabilities().iter(),
        }
    }
}

pub struct StochasticNondeterministicFiniteAutomatonIterator<'a> {
    it_sources: std::slice::Iter<'a, usize>,
    it_targets: std::slice::Iter<'a, usize>,
    it_activities: std::slice::Iter<'a, Option<Activity>>,
    it_probabilities: std::slice::Iter<'a, Fraction>,
}

impl<'a> Iterator for StochasticNondeterministicFiniteAutomatonIterator<'a> {
    type Item = (&'a usize, &'a usize, &'a Option<Activity>, &'a Fraction);

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(source) = self.it_sources.next() {
            let target = self.it_targets.next().unwrap();
            let activity = self.it_activities.next().unwrap();
            let probability = self.it_probabilities.next().unwrap();
            let result = Some((source, target, activity, probability));
            result
        } else {
            None
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.it_sources.size_hint()
    }
}

impl<'a> IntoIterator for &'a mut StochasticNondeterministicFiniteAutomaton {
    type Item = (&'a usize, &'a usize, &'a Option<Activity>, &'a mut Fraction);

    type IntoIter = StochasticNondeterministicFiniteAutomatonMutIterator<'a>;

    fn into_iter(self) -> Self::IntoIter {
        Self::IntoIter {
            it_sources: self.sources.iter(),
            it_targets: self.targets.iter(),
            it_activities: self.activities.iter(),
            it_probabilities: self.probabilities.iter_mut(),
        }
    }
}

pub struct StochasticNondeterministicFiniteAutomatonMutIterator<'a> {
    it_sources: std::slice::Iter<'a, usize>,
    it_targets: std::slice::Iter<'a, usize>,
    it_activities: std::slice::Iter<'a, Option<Activity>>,
    it_probabilities: std::slice::IterMut<'a, Fraction>,
}

impl<'a> Iterator for StochasticNondeterministicFiniteAutomatonMutIterator<'a> {
    type Item = (&'a usize, &'a usize, &'a Option<Activity>, &'a mut Fraction);

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(source) = self.it_sources.next() {
            let target = self.it_targets.next().unwrap();
            let activity = self.it_activities.next().unwrap();
            let probability = self.it_probabilities.next().unwrap();
            let result = Some((source, target, activity, probability));
            result
        } else {
            None
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.it_sources.size_hint()
    }
}

#[cfg(test)]
mod tests {
    use crate::{Graphable, StochasticNondeterministicFiniteAutomaton};
    use itertools::Itertools;
    use std::fs;

    #[test]
    fn snfa_outgoing_iter() {
        let fin = fs::read_to_string("testfiles/aa-ab-ba.snfa").unwrap();
        let dfm = fin
            .parse::<StochasticNondeterministicFiniteAutomaton>()
            .unwrap();

        let mut it = dfm.outgoing_edges(0);
        assert_eq!(it.try_len().unwrap(), 3);
        assert!(it.next().is_some());
        assert!(it.next().is_some());
        assert!(it.next().is_some());
        assert!(it.next().is_none());

        let mut it = dfm.outgoing_edges(1);
        assert_eq!(it.try_len().unwrap(), 1);
        assert!(it.next().is_some());
        assert!(it.next().is_none());
    }

    #[test]
    fn snfa_graph() {
        let fin = fs::read_to_string("testfiles/loop(a,tau)and(bc).snfa").unwrap();
        let dfm = fin
            .parse::<StochasticNondeterministicFiniteAutomaton>()
            .unwrap();

        assert_eq!(dfm.number_of_states(), 7);
        assert_eq!(dfm.number_of_transitions(), 8);

        dfm.to_dot().unwrap();
    }
}
