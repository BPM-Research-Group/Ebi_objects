use crate::{
    Activity, ActivityKey, ActivityKeyTranslator, EbiObject, Exportable, Graphable, HasActivityKey,
    Importable, Infoable, TranslateActivityKey, json,
    traits::{
        graphable,
        importable::{ImporterParameter, ImporterParameterValues, from_string},
    },
};
use anyhow::{Context, Result, anyhow};
use ebi_arithmetic::{Fraction, One, Signed};
use ebi_derive::ActivityKey;
use layout::topo::layout::VisualGraph;
use serde_json::Value;
use std::{
    cmp::{Ordering, max},
    collections::HashMap,
    fmt,
    io::BufRead,
};

#[derive(Debug, ActivityKey, Clone)]
pub struct StochasticDeterministicFiniteAutomaton {
    pub activity_key: ActivityKey,
    pub initial_state: Option<usize>,
    pub max_state: usize,
    pub sources: Vec<usize>,          //transition -> source of arc
    pub targets: Vec<usize>,          //transition -> target of arc
    pub activities: Vec<Activity>,    //transition -> activity of arc (every transition is labelled)
    pub probabilities: Vec<Fraction>, //transition -> probability of arc
    pub terminating_probabilities: Vec<Fraction>, //state -> termination probability
}

impl StochasticDeterministicFiniteAutomaton {
    /**
     * Creates a new SDFA with an initial state. That is, it will have the stochastic language with the empty trace.
     */
    pub fn new() -> Self {
        Self {
            activity_key: ActivityKey::new(),
            max_state: 0,
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

    pub fn get_activities(&self) -> &Vec<Activity> {
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

    fn ensure_states(&mut self, new_max_state: usize) {
        if new_max_state > self.max_state {
            self.terminating_probabilities
                .extend(vec![Fraction::one(); new_max_state - self.max_state]);
            self.max_state = new_max_state;

            assert!(self.terminating_probabilities.len() == self.max_state + 1)
        }
    }

    /// Inserts a transition; will fail if determinism or stochastic perspective is violated.
    pub fn add_transition(
        &mut self,
        source: usize,
        activity: Activity,
        target: usize,
        probability: Fraction,
    ) -> Result<()> {
        self.ensure_states(max(source, target));

        let (found, from) =
            self.binary_search(source, self.activity_key.get_id_from_activity(activity));
        if found {
            //edge already present
            Err(anyhow!(
                "tried to insert edge from {} to {}, which would violate the determinism of the SDFA",
                source,
                target
            ))
        } else {
            self.sources.insert(from, source);
            self.targets.insert(from, target);
            self.activities.insert(from, activity);
            self.terminating_probabilities[source] -= &probability;
            self.probabilities.insert(from, probability);

            if self.terminating_probabilities[source].is_negative() {
                Err(anyhow!(
                    "tried to insert edge from {} to {}, which brings the sum outgoing probability of the source state (1-{}) above 1",
                    source,
                    target,
                    self.terminating_probabilities[source]
                ))
            } else {
                Ok(())
            }
        }
    }

    pub fn number_of_transitions(&self) -> usize {
        self.sources.len()
    }

    /**
     * Adds the probability to the transition. Returns the target state, which may be new.
     */
    pub fn take_or_add_transition(
        &mut self,
        source_state: usize,
        activity: Activity,
        probability: Fraction,
    ) -> usize {
        self.terminating_probabilities[source_state] -= &probability;

        let (found, transition) = self.binary_search(
            source_state,
            self.activity_key.get_id_from_activity(activity),
        );
        if found {
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

    pub fn get_initial_state(&self) -> Option<usize> {
        self.initial_state
    }

    pub fn get_termination_probability(&self, state: usize) -> &Fraction {
        &self.terminating_probabilities[state]
    }

    pub fn add_state(&mut self) -> usize {
        self.max_state += 1;
        self.terminating_probabilities.push(Fraction::one());
        return self.max_state;
    }

    pub fn get_max_state(&self) -> usize {
        self.max_state
    }

    fn compare(source1: usize, activity1: usize, source2: usize, activity2: Activity) -> Ordering {
        if source1 < source2 {
            return Ordering::Greater;
        } else if source1 > source2 {
            return Ordering::Less;
        } else if activity2 > activity1 {
            return Ordering::Greater;
        } else if activity2 < activity1 {
            return Ordering::Less;
        } else {
            return Ordering::Equal;
        }
    }

    pub fn binary_search(&self, source: usize, activity: usize) -> (bool, usize) {
        if self.sources.is_empty() {
            return (false, 0);
        }

        let mut size = self.sources.len();
        let mut left = 0;
        let mut right = size;
        while left < right {
            let mid = left + size / 2;

            let cmp = Self::compare(source, activity, self.sources[mid], self.activities[mid]);

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

    /// Returns an iterator over the outgoing edges of `source`.
    pub fn outgoing_edges(
        &'_ self,
        source: usize,
    ) -> StochasticDeterministicFiniteAutomatonIterator<'_> {
        let (_, start) = self.binary_search(source, 0);
        let (_, end) = self.binary_search(source, usize::MAX);
        StochasticDeterministicFiniteAutomatonIterator {
            it_sources: self.get_sources()[start..end].iter(),
            it_targets: self.get_targets()[start..end].iter(),
            it_activities: self.get_activities()[start..end].iter(),
            it_probabilities: self.get_probabilities()[start..end].iter(),
        }
    }
}

impl TranslateActivityKey for StochasticDeterministicFiniteAutomaton {
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
            self.add_transition(
                source,
                translator.translate_activity(&activity),
                target,
                probability,
            )
            .unwrap(); //by invariant, this can only fail if the original model was incorrect already
        }
        self.activity_key = to_activity_key.clone();
    }
}

impl Importable for StochasticDeterministicFiniteAutomaton {
    const FILE_FORMAT_SPECIFICATION_LATEX: &str = "A stochastic deterministic finite automaton is a JSON structure with the top level being an object.
    This object contains the following key-value pairs:
    \\begin{itemize}
    \\item \\texttt{initialState} being the index of the initial state. This field is optional: if omitted, the SDFA has an empty stochastic language.
    \\item \\texttt{transitions} being a list of transitions. 
    Each transition is an object with \\texttt{from} being the source state index of the transition, 
    \\texttt{to} being the target state index of the transition, 
    \\texttt{label} being the activity of the transition, and
    \\texttt{prob} being the probability of the transition (may be given as a fraction in a string or a float value. Must be $\\leq 1$). 
    Silent transitions are not supported.
    The file format supports deadlocks and livelocks.
    The probability that a trace terminates in a state is 1 - the sum probability of the outgoing transitions of the state.
    \\end{itemize}
    For instance:
    \\lstinputlisting[language=json, style=boxed]{../testfiles/aa-ab-ba.sdfa}";

    const IMPORTER_PARAMETERS: &[ImporterParameter] = &[];

    fn import_as_object(
        reader: &mut dyn BufRead,
        parameter_values: &ImporterParameterValues,
    ) -> Result<EbiObject> {
        Ok(EbiObject::StochasticDeterministicFiniteAutomaton(
            Self::import(reader, parameter_values)?,
        ))
    }

    fn import(reader: &mut dyn BufRead, _: &ImporterParameterValues) -> Result<Self>
    where
        Self: Sized,
    {
        let json: Value = serde_json::from_reader(reader)?;

        let mut result = StochasticDeterministicFiniteAutomaton::new();

        result.set_initial_state(json::read_field_number(&json, "initialState").ok());
        let jtrans = json::read_field_list(&json, "transitions")
            .context("failed to read list of transitions")?;
        for (i, jtransition) in jtrans.iter().enumerate() {
            let from = json::read_field_number(jtransition, "from")
                .with_context(|| format!("could not read source of transition {}", i))?;
            let to = json::read_field_number(jtransition, "to")
                .with_context(|| format!("could not read destination of transition {}", i))?;
            let label = json::read_field_string(jtransition, "label")
                .with_context(|| format!("could not read label of transition {}", i))?;
            let activity = result.activity_key.process_activity(label.as_str());
            let probability = json::read_field_fraction(jtransition, "prob")
                .with_context(|| format!("could not read probability on transition {}", i))?;

            result.add_transition(from, activity, to, probability)?;
        }

        return Ok(result);
    }
}
from_string!(StochasticDeterministicFiniteAutomaton);

impl Exportable for StochasticDeterministicFiniteAutomaton {
    fn export_from_object(object: EbiObject, f: &mut dyn std::io::Write) -> Result<()> {
        match object {
            EbiObject::StochasticDeterministicFiniteAutomaton(sdfa) => sdfa.export(f),
            EbiObject::FiniteStochasticLanguage(slang) => Into::<Self>::into(slang).export(f),
            EbiObject::EventLog(log) => Into::<Self>::into(log).export(f),
            EbiObject::EventLogTraceAttributes(log) => Into::<Self>::into(log).export(f),
            EbiObject::EventLogXes(log) => Into::<Self>::into(log).export(f),
            EbiObject::EventLogCsv(log) => Into::<Self>::into(log).export(f),
            _ => Err(anyhow!(
                "Cannot export {} {} as an SDFA.",
                object.get_type().get_article(),
                object.get_type()
            )),
        }
    }

    fn export(&self, f: &mut dyn std::io::Write) -> Result<()> {
        Ok(write!(f, "{}", self)?)
    }
}

impl Infoable for StochasticDeterministicFiniteAutomaton {
    fn info(&self, f: &mut impl std::io::Write) -> Result<()> {
        writeln!(f, "Number of states\t{}", self.max_state)?;
        writeln!(f, "Number of transitions\t{}", self.sources.len())?;
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

impl fmt::Display for StochasticDeterministicFiniteAutomaton {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{{")?;
        if let Some(state) = self.get_initial_state() {
            writeln!(f, "\"initialState\": {},", state)?;
        }
        writeln!(f, "\"transitions\": [")?;
        for pos in 0..self.sources.len() {
            write!(
                f,
                "{{\"from\":{},\"to\":{},\"label\":\"{}\",\"prob\":\"{}\"}}",
                self.sources[pos],
                self.targets[pos],
                self.activity_key.get_activity_label(&self.activities[pos]),
                self.probabilities[pos]
            )?;
            if pos + 1 == self.sources.len() {
                writeln!(f, "")?;
            } else {
                writeln!(f, ",")?;
            }
        }
        writeln!(f, "]}}")?;
        Ok(())
    }
}

impl Graphable for StochasticDeterministicFiniteAutomaton {
    fn to_dot(&self) -> Result<layout::topo::layout::VisualGraph> {
        log::info!("to_dot for StochasticDeterministicFiniteAutomaton");
        let mut graph = VisualGraph::new(layout::core::base::Orientation::LeftToRight);

        let mut places = vec![];
        for state in 0..=self.max_state {
            places.push(graphable::create_place(
                &mut graph,
                &format!("{}", self.terminating_probabilities[state]),
            ));
            // places.push(<dyn Dottable>::create_place(&mut graph, ""));
        }

        for pos in 0..self.sources.len() {
            let source = places[self.sources[pos]];
            let target = places[self.targets[pos]];
            let probability = &self.probabilities[pos];
            let activity = self.activity_key.get_activity_label(&self.activities[pos]);

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

impl<'a> IntoIterator for &'a StochasticDeterministicFiniteAutomaton {
    type Item = (&'a usize, &'a usize, &'a Activity, &'a Fraction);

    type IntoIter = StochasticDeterministicFiniteAutomatonIterator<'a>;

    fn into_iter(self) -> Self::IntoIter {
        Self::IntoIter {
            it_sources: self.get_sources().iter(),
            it_targets: self.get_targets().iter(),
            it_activities: self.get_activities().iter(),
            it_probabilities: self.get_probabilities().iter(),
        }
    }
}

pub struct StochasticDeterministicFiniteAutomatonIterator<'a> {
    it_sources: std::slice::Iter<'a, usize>,
    it_targets: std::slice::Iter<'a, usize>,
    it_activities: std::slice::Iter<'a, Activity>,
    it_probabilities: std::slice::Iter<'a, Fraction>,
}

impl<'a> Iterator for StochasticDeterministicFiniteAutomatonIterator<'a> {
    type Item = (&'a usize, &'a usize, &'a Activity, &'a Fraction);

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

impl<'a> IntoIterator for &'a mut StochasticDeterministicFiniteAutomaton {
    type Item = (&'a usize, &'a usize, &'a Activity, &'a mut Fraction);

    type IntoIter = StochasticDeterministicFiniteAutomatonMutIterator<'a>;

    fn into_iter(self) -> Self::IntoIter {
        Self::IntoIter {
            it_sources: self.sources.iter(),
            it_targets: self.targets.iter(),
            it_activities: self.activities.iter(),
            it_probabilities: self.probabilities.iter_mut(),
        }
    }
}

pub struct StochasticDeterministicFiniteAutomatonMutIterator<'a> {
    it_sources: std::slice::Iter<'a, usize>,
    it_targets: std::slice::Iter<'a, usize>,
    it_activities: std::slice::Iter<'a, Activity>,
    it_probabilities: std::slice::IterMut<'a, Fraction>,
}

impl<'a> Iterator for StochasticDeterministicFiniteAutomatonMutIterator<'a> {
    type Item = (&'a usize, &'a usize, &'a Activity, &'a mut Fraction);

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
    use crate::StochasticDeterministicFiniteAutomaton;
    use itertools::Itertools;
    use std::fs;

    #[test]
    fn sdfa_outgoing_iter() {
        let fin = fs::read_to_string("testfiles/aa-ab-ba.sdfa").unwrap();
        let dfm = fin
            .parse::<StochasticDeterministicFiniteAutomaton>()
            .unwrap();

        let mut it = dfm.outgoing_edges(0);
        assert_eq!(it.try_len().unwrap(), 2);
        assert!(it.next().is_some());
        assert!(it.next().is_some());
        assert!(it.next().is_none());

        let mut it = dfm.outgoing_edges(1);
        assert_eq!(it.try_len().unwrap(), 2);
        assert!(it.next().is_some());
        assert!(it.next().is_some());
        assert!(it.next().is_none());
    }
}
