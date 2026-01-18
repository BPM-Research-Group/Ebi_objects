use crate::{
    Activity, ActivityKey, ActivityKeyTranslator, EbiObject, Exportable, Graphable, HasActivityKey,
    Importable, Infoable, TranslateActivityKey,
    line_reader::LineReader,
    traits::{
        graphable,
        importable::{ImporterParameter, ImporterParameterValues},
    },
};
use anyhow::{Context, Result, anyhow};
use ebi_arithmetic::{EbiMatrix, Fraction, FractionMatrix, Inversion, One, Signed, Zero};
use ebi_derive::ActivityKey;
use layout::topo::layout::VisualGraph;
use std::{
    collections::{HashMap, VecDeque},
    fmt::Display,
};

pub const HEADER: &str = "stochastic non-deterministic finite automaton";

/// The semantics are: starting in the `initial` state we either terminate with
/// probability `p_final` or follow one of the outgoing transitions chosen
/// according to their probabilities and move to the target state.
#[derive(Clone, Debug, ActivityKey)]
pub struct StochasticNondeterministicFiniteAutomaton {
    pub activity_key: ActivityKey,
    pub states: Vec<State>,

    /// If the initial state is None, then the language is empty
    pub initial_state: Option<usize>,
}

/// A state in a stochastic non-deterministic finite automaton
#[derive(Clone, Debug)]
pub struct State {
    pub transitions: Vec<Transition>,

    /// Probability of terminating in this state.
    /// The sum of termination_probability and the probability of each transition must equal 1.
    pub termination_probability: Fraction,
}

/// A transition in a stochastic non-deterministic finite automaton
#[derive(Clone, Debug)]
pub struct Transition {
    pub target: usize,
    pub label: Option<Activity>,
    pub probability: Fraction,
}

impl StochasticNondeterministicFiniteAutomaton {
    /// Create an empty SNFA with a single initial / final state
    pub fn new() -> Self {
        Self {
            states: vec![State {
                transitions: vec![],
                termination_probability: Fraction::one(),
            }],
            initial_state: Some(0),
            activity_key: ActivityKey::new(),
        }
    }

    /// Ensures that a state with index `idx` exists, extending the vector if necessary.
    fn ensure_state(&mut self, idx: usize) {
        while self.states.len() <= idx {
            self.states.push(State {
                transitions: vec![],
                termination_probability: Fraction::zero(),
            });
        }
    }

    /// Adds a transition. The caller must make sure that outgoing probabilities of each state sum up to <= 1.
    pub fn add_transition(
        &mut self,
        source: usize,
        activity: Option<Activity>,
        target: usize,
        probability: Fraction,
    ) {
        self.ensure_state(source);
        self.ensure_state(target);
        self.states[source].transitions.push(Transition {
            target,
            label: activity,
            probability,
        });
    }

    /// Sets the final-probability of a state.
    pub fn set_final_probability(&mut self, state: usize, p_final: Fraction) {
        self.ensure_state(state);
        self.states[state].termination_probability = p_final;
    }

    /**
     * Adds the probability to the transition. Returns the target state, which may be new.
     * The caller is responsible for updating the termination probability.
     */
    pub fn take_or_add_transition(
        &mut self,
        source: usize,
        activity: Activity,
        probability: Fraction,
    ) -> usize {
        //find the transition
        for transition in self.states[source].transitions.iter_mut() {
            if transition.label == Some(activity) {
                transition.probability += probability;
                return transition.target;
            }
        }

        //label not found, add a new state and transition
        let target = self.states.len();
        self.states.push(State {
            transitions: vec![],
            termination_probability: Fraction::zero(),
        });
        self.states[source].transitions.push(Transition {
            target,
            label: Some(activity),
            probability,
        });
        return target;
    }

    /**
     * Scales the outgoing probabilities of the state.
     */
    pub fn scale_outgoing_probabilities(&mut self) {
        for state in self.states.iter_mut() {
            let mut sum = state
                .transitions
                .iter()
                .map(|transition| &transition.probability)
                .sum::<Fraction>();
            sum += &state.termination_probability;

            state
                .transitions
                .iter_mut()
                .for_each(|transition| transition.probability /= &sum);

            state.termination_probability /= sum;
        }
    }

    /// Returns the number of states.
    pub fn len(&self) -> usize {
        self.states.len()
    }

    pub fn is_empty(&self) -> bool {
        self.states.is_empty()
    }

    /// Removes all tau-transitions (those whose label is None)
    /// from the automaton while preserving its stochastic behaviour.
    ///
    /// The implementation follows Mohri's two–phase epsilon-removal scheme
    /// adapted to the probabilistic semiring.
    pub fn remove_tau_transitions(&mut self) -> Result<()> {
        let n = self.len();
        if n == 0 {
            return Ok(());
        }

        let one = Fraction::one();
        let zero = Fraction::zero();

        //chech whether the SLPN has an initial state
        let initial_state = if let Some(initial) = self.initial_state {
            initial
        } else {
            return Ok(());
        };

        // Build tau-only adjacency with weights
        let mut tau_adj: Vec<Vec<(usize, Fraction)>> = vec![vec![]; n];
        for p in 0..n {
            for t in &self.states[p].transitions {
                if t.label.is_none() {
                    tau_adj[p].push((t.target, t.probability.clone()));
                }
            }
        }

        // Compute tau-closure matrix d[p,q]
        // Initially d = I (identity)
        let mut d: Vec<Vec<Fraction>> = vec![vec![zero.clone(); n]; n];
        // Track rows that already contain non-zero entries -> lets us skip all-zero rows later
        let mut row_nonzero: Vec<bool> = vec![false; n];
        for i in 0..n {
            d[i][i] = one.clone();
        }

        // Compute SCCs of tau-graph and their reverse topological order
        let sccs = compute_scc(n, &tau_adj);
        // Create map state -> component id
        let mut comp_of = vec![0usize; n];
        for (cid, comp) in sccs.iter().enumerate() {
            for &v in comp {
                comp_of[v] = cid;
            }
        }

        // Build DAG edges from each component to its successor components
        let mut dag_succ: Vec<Vec<usize>> = vec![vec![]; sccs.len()];
        for u in 0..n {
            for &(v, _) in &tau_adj[u] {
                let cu = comp_of[u];
                let cv = comp_of[v];
                if cu != cv && !dag_succ[cu].contains(&cv) {
                    dag_succ[cu].push(cv);
                }
            }
        }

        // Obtain reverse topological order via DFS on DAG
        let mut visited = vec![false; sccs.len()];
        let mut rev_topo: Vec<usize> = Vec::new();
        fn dfs(u: usize, dag: &Vec<Vec<usize>>, vis: &mut Vec<bool>, out: &mut Vec<usize>) {
            if vis[u] {
                return;
            }
            vis[u] = true;
            for &v in &dag[u] {
                dfs(v, dag, vis, out);
            }
            out.push(u);
        }
        for cid in 0..sccs.len() {
            dfs(cid, &dag_succ, &mut visited, &mut rev_topo);
        }

        // Process SCCs in reverse topological order
        for &cid in &rev_topo {
            let comp = &sccs[cid];
            let m = comp.len();
            if m == 1 && tau_adj[comp[0]].is_empty() {
                // single vertex with no self-loop -> nothing to invert, d already has identity
            } else {
                // Build (I-W) for this component
                let mut matrix = FractionMatrix::new(m, m);
                for (i_idx, &i) in comp.iter().enumerate() {
                    matrix.set(i_idx, i_idx, Fraction::one());
                    for &(j, ref w) in &tau_adj[i] {
                        if comp_of[j] == cid {
                            let j_idx = comp.iter().position(|&x| x == j).unwrap();
                            matrix.set(
                                i_idx,
                                j_idx,
                                &matrix.get(i_idx, j_idx).unwrap() - &w.clone(),
                            );
                        }
                    }
                }
                let d_local = matrix
                    .invert()
                    .with_context(|| "tau-removal: singular (I-W) in SCC")?;
                // store intra-component closure
                for (i_idx, &i) in comp.iter().enumerate() {
                    for (j_idx, &j) in comp.iter().enumerate() {
                        d[i][j] = d_local.get(i_idx, j_idx).unwrap().clone();
                    }
                    row_nonzero[i] = true;
                }
            }

            // Propagate to successor components (inter-SCC tau-edges)
            for &q in comp {
                for &(r, ref w_qr) in &tau_adj[q] {
                    if comp_of[r] == cid {
                        continue;
                    }
                    // For every s with d[r][s] > 0 (s already processed)
                    for p in 0..n {
                        if !row_nonzero[p] {
                            continue;
                        }
                        if d[p][q].is_zero() {
                            continue;
                        }
                        let prefix = &d[p][q] * &w_qr.clone();
                        if prefix.is_zero() {
                            continue;
                        }

                        for s in 0..n {
                            let d_rs = d[r][s].clone();
                            if d_rs.is_zero() {
                                continue;
                            }
                            let add = &prefix * &d_rs;
                            if add.is_zero() {
                                continue;
                            }

                            d[p][s] += add;
                        }
                    }
                }
            }
        }

        // Transform graph
        let mut new_transitions: Vec<Vec<Transition>> = vec![vec![]; n];
        let mut new_p_final: Vec<Fraction> = vec![zero.clone(); n];

        for q in 0..n {
            for t in &self.states[q].transitions {
                if !t.label.is_none() {
                    let tprob = t.probability.clone();
                    for p in 0..n {
                        if d[p][q].is_zero() {
                            continue;
                        }
                        let prob = &d[p][q] * &tprob;
                        new_transitions[p].push(Transition {
                            target: t.target,
                            label: t.label.clone(),
                            probability: prob,
                        });
                    }
                }
            }
            if !self.states[q].termination_probability.is_zero() {
                for p in 0..n {
                    if d[p][q].is_zero() {
                        continue;
                    }
                    new_p_final[p] = &new_p_final[p]
                        + &(&d[p][q] * &self.states[q].termination_probability.clone());
                }
            }
        }

        // Merge parallel edges
        for p in 0..n {
            let mut map: HashMap<(usize, Option<Activity>), Fraction> = HashMap::new();
            for tr in new_transitions[p].drain(..) {
                *map.entry((tr.target, tr.label.clone()))
                    .or_insert_with(Fraction::zero) += tr.probability.clone();
            }
            new_transitions[p] = map
                .into_iter()
                .map(|((tgt, lbl), prob)| Transition {
                    target: tgt,
                    label: lbl,
                    probability: prob,
                })
                .collect();
        }

        // Write back
        for p in 0..n {
            self.states[p].transitions = new_transitions[p].clone();
            self.states[p].termination_probability = new_p_final[p].clone();
        }

        // Trim unreachable states (reachable over visible edges)
        {
            let mut reachable = vec![false; self.states.len()];
            let mut queue = VecDeque::new();
            queue.push_back(initial_state);
            while let Some(u) = queue.pop_front() {
                if reachable[u] {
                    continue;
                }
                reachable[u] = true;
                for tr in &self.states[u].transitions {
                    if tr.label.is_none() {
                        continue;
                    }
                    queue.push_back(tr.target);
                }
            }
            if reachable.iter().any(|&r| !r) {
                let mut map = vec![None; self.states.len()];
                let mut new_states: Vec<State> = Vec::new();
                for (old_idx, st) in self.states.iter().enumerate() {
                    if reachable[old_idx] {
                        let new_idx = new_states.len();
                        map[old_idx] = Some(new_idx);
                        new_states.push(State {
                            transitions: Vec::new(),
                            termination_probability: st.termination_probability.clone(),
                        });
                    }
                }
                for (old_idx, st) in self.states.iter().enumerate() {
                    if let Some(new_src) = map[old_idx] {
                        for tr in &st.transitions {
                            if let Some(new_tgt) = map[tr.target] {
                                new_states[new_src].transitions.push(Transition {
                                    target: new_tgt,
                                    label: tr.label.clone(),
                                    probability: tr.probability.clone(),
                                });
                            }
                        }
                    }
                }
                self.states = new_states;
                self.initial_state = Some(map[initial_state].unwrap());
            }
        }

        // Check row sums
        for (i, st) in self.states.iter().enumerate() {
            let mut row = st.termination_probability.clone();
            for tr in &st.transitions {
                row += tr.probability.clone();
            }
            debug_assert!(
                row == Fraction::one(),
                "row sum ≠ 1 after tau-removal in state {}",
                i
            );
        }

        Ok(())
    }
}

impl TranslateActivityKey for StochasticNondeterministicFiniteAutomaton {
    fn translate_using_activity_key(&mut self, to_activity_key: &mut ActivityKey) {
        let translator = ActivityKeyTranslator::new(&self.activity_key, to_activity_key);
        self.states.iter_mut().for_each(|state| {
            state.transitions.iter_mut().for_each(|transition| {
                if let Some(activity) = transition.label.as_mut() {
                    *activity = translator.translate_activity(&activity)
                }
            })
        });
        self.activity_key = to_activity_key.clone();
    }
}

impl Display for StochasticNondeterministicFiniteAutomaton {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}", HEADER)?;
        writeln!(f, "# initial state\n{:?}", self.initial_state)?;
        writeln!(f, "# number of states\n{}", self.states.len())?;
        for (id, state) in self.states.iter().enumerate() {
            writeln!(f, "# state {}", id)?;
            writeln!(
                f,
                "# termination probability\n{}",
                state.termination_probability
            )?;
            writeln!(f, "# number of transitions\n{}", state.transitions.len())?;
            for transition in &state.transitions {
                writeln!(f, "# target state\n{}", transition.target)?;
                if let Some(activity) = transition.label {
                    writeln!(
                        f,
                        "# transition label\nlabel {}",
                        self.activity_key.deprocess_activity(&activity)
                    )?;
                } else {
                    writeln!(f, "# transition label\nsilent")?;
                }
                writeln!(f, "# transition probability\n{}", transition.probability)?;
            }
        }
        Ok(())
    }
}

impl Importable for StochasticNondeterministicFiniteAutomaton {
    const IMPORTER_PARAMETERS: &[ImporterParameter] = &[];

    const FILE_FORMAT_SPECIFICATION_LATEX: &str = "A stochastic non-deterministic finite automaton is a line-based structure. Lines starting with a \\# are ignored.
    This first line is exactly `stochastic non-deterministic finite automaton'.
    The second line is the initial state of the SNFA, or the word `None'.
    The third line is the number of states in the SNFA.
    The lines thereafter contain the states: each state starts with the termination probability.
    Then, for each outgoing transition of the state, the following lines are next: 
    (i) the target state of the transition;
    (ii) the word `silent' or the word `label' followed by a space and the name of the activity with which the transition is labelled;
    (iii) the probability of the transition.
    
    For instance:
    \\lstinputlisting[language=ebilines, style=boxed]{../testfiles/aa-ab-ba.snfa}";

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

        let mut activity_key = ActivityKey::new();

        //read initial state
        let initial_state = lreader
            .next_line_string()
            .with_context(|| "failed to read initial state")?;
        let initial_state = if &initial_state.trim_start().to_lowercase()[..] == "none" {
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

        //read states
        let mut states = Vec::with_capacity(number_of_states);
        for state_nr in 0..number_of_states {
            //read termination probability
            let termination_probability = lreader.next_line_weight().with_context(|| {
                format!(
                    "could not read termination probability of state {}",
                    state_nr
                )
            })?;
            if !termination_probability.is_positive() {
                return Err(anyhow!(
                    "termination probability of state {} is not positive: {}",
                    state_nr,
                    termination_probability
                ));
            }
            let mut sum = termination_probability.clone();

            //read number of transitions
            let number_of_transitions = lreader.next_line_index().with_context(|| {
                format!("could not read number of transitions of state {}", state_nr)
            })?;

            //read transitions
            let mut transitions = Vec::with_capacity(number_of_transitions);
            for transition_nr in 0..number_of_transitions {
                //read target
                let target = lreader.next_line_index().with_context(|| {
                    format!(
                        "could not read target state of state {} transition {}",
                        state_nr, transition_nr
                    )
                })?;

                //read label
                let label_line = lreader.next_line_string().with_context(|| {
                    format!(
                        "failed to read label of state {} transition {}",
                        state_nr, transition_nr
                    )
                })?;
                let label = if label_line.trim_start().starts_with("label ") {
                    let label = label_line.trim_start()[6..].to_string();
                    let activity = activity_key.process_activity(&label);
                    Some(activity)
                } else {
                    None
                };

                //read probability
                let probability = lreader.next_line_weight().with_context(|| {
                    format!(
                        "could not read probability of state {} transition {}",
                        state_nr, transition_nr
                    )
                })?;
                if !probability.is_positive() {
                    return Err(anyhow!(
                        "probability of state {} transition {} is not positive: {}",
                        state_nr,
                        transition_nr,
                        probability
                    ));
                }
                sum += &probability;

                transitions.push(Transition {
                    target,
                    label,
                    probability,
                });
            }

            //verify that sum of outgoing edges is ones
            if (sum > Fraction::one() || sum < Fraction::one()) && !sum.is_one() {
                //avoid rounding errors in approximate mode
                return Err(anyhow!(
                    "probabilities in state {} sum to {}, which is greater than 1",
                    state_nr,
                    sum
                ));
            }

            states.push(State {
                transitions,
                termination_probability,
            });
        }

        Ok(Self {
            activity_key,
            states,
            initial_state,
        })
    }
}

impl Infoable for StochasticNondeterministicFiniteAutomaton {
    fn info(&self, f: &mut impl std::io::Write) -> Result<()> {
        writeln!(f, "Number of states\t{}", self.states.len())?;
        writeln!(
            f,
            "Number of transitions\t{}",
            self.states
                .iter()
                .map(|state| state.transitions.len())
                .sum::<usize>()
        )?;
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
        for state in &self.states {
            places.push(graphable::create_place(
                &mut graph,
                &format!("{}", state.termination_probability),
            ));
        }

        for (id, state) in self.states.iter().enumerate() {
            let source = places[id];
            for transition in &state.transitions {
                let target = places[transition.target];
                let probability = &transition.probability;
                let activity = if let Some(activity) = transition.label {
                    self.activity_key.get_activity_label(&activity)
                } else {
                    ""
                };

                graphable::create_edge(
                    &mut graph,
                    &source,
                    &target,
                    &format!("{}, {}", activity, probability.to_string()),
                );
            }
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
            EbiObject::EventLog(log) => Into::<Self>::into(log).export(f),
            EbiObject::EventLogTraceAttributes(log) => Into::<Self>::into(log).export(f),
            EbiObject::EventLogXes(log) => Into::<Self>::into(log).export(f),
            EbiObject::EventLogCsv(log) => Into::<Self>::into(log).export(f),
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

/// Kosaraju algorithm to compute SCCs of a directed weighted graph (adjacency list ignores weights).
fn compute_scc(n: usize, adj: &Vec<Vec<(usize, Fraction)>>) -> Vec<Vec<usize>> {
    // 1st pass: order by finish time using DFS on original graph
    let mut visited = vec![false; n];
    let mut order: Vec<usize> = Vec::with_capacity(n);
    fn dfs1(
        u: usize,
        adj: &Vec<Vec<(usize, Fraction)>>,
        vis: &mut Vec<bool>,
        order: &mut Vec<usize>,
    ) {
        if vis[u] {
            return;
        }
        vis[u] = true;
        for &(v, _) in &adj[u] {
            dfs1(v, adj, vis, order);
        }
        order.push(u);
    }
    for v in 0..n {
        dfs1(v, adj, &mut visited, &mut order);
    }

    // Build reverse graph
    let mut radj: Vec<Vec<usize>> = vec![vec![]; n];
    for u in 0..n {
        for &(v, _) in &adj[u] {
            radj[v].push(u);
        }
    }

    // 2nd pass: DFS on reverse graph in reverse finish order
    let mut comp_id = vec![None; n];
    let mut comps: Vec<Vec<usize>> = Vec::new();
    fn dfs2(
        u: usize,
        radj: &Vec<Vec<usize>>,
        comp: &mut Vec<usize>,
        comp_id: &mut Vec<Option<usize>>,
        cid: usize,
    ) {
        if comp_id[u].is_some() {
            return;
        }
        comp_id[u] = Some(cid);
        comp.push(u);
        for &v in &radj[u] {
            dfs2(v, radj, comp, comp_id, cid);
        }
    }
    let mut cid = 0;
    while let Some(u) = order.pop() {
        if comp_id[u].is_none() {
            comps.push(Vec::new());
            dfs2(u, &radj, comps.last_mut().unwrap(), &mut comp_id, cid);
            cid += 1;
        }
    }
    comps
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;

    /// frac!(n,d)  ->  Fraction::from((n,d))
    macro_rules! frac {
        ($n:expr , $d:expr) => {
            Fraction::from(($n as u32, $d as u32))
        };
    }

    /// Collapse parallel visible edges into (src,label,dst) -> probability.
    fn collect(states: &[State]) -> HashMap<(usize, Option<Activity>, usize), Fraction> {
        let mut m = HashMap::new();
        for (src, st) in states.iter().enumerate() {
            for tr in &st.transitions {
                *m.entry((src, tr.label.clone(), tr.target))
                    .or_insert_with(Fraction::zero) += tr.probability.clone();
            }
        }
        m
    }

    /// Ensure every state row sums exactly to 1.
    fn assert_rows_sum_to_one(states: &[State]) {
        for (i, st) in states.iter().enumerate() {
            let mut row = st.termination_probability.clone();
            for tr in &st.transitions {
                row += tr.probability.clone();
            }
            assert_eq!(row, frac!(1, 1), "row sum != 1 in state {}", i);
        }
    }

    // 1. detailed self-loop + cycle test
    #[test]
    fn tau_removal_self_loop_and_cycle_rows_sum_to_one() {
        // tau self-loop on 0 (1/5) and cycle between 1 and 2 (1/4,1/2)
        let mut a = StochasticNondeterministicFiniteAutomaton::new();
        for s in 0..3 {
            a.ensure_state(s);
        }
        a.set_final_probability(0, Fraction::zero());

        a.add_transition(0, None, 0, frac!(1, 5));
        a.add_transition(1, None, 2, frac!(1, 4));
        a.add_transition(2, None, 1, frac!(1, 2));

        let acta = Some(a.activity_key.process_activity("a"));
        let actb = Some(a.activity_key.process_activity("b"));
        let actc = Some(a.activity_key.process_activity("c"));
        let actd = Some(a.activity_key.process_activity("d"));

        a.add_transition(0, acta, 0, frac!(3, 5));
        a.add_transition(0, actb, 1, frac!(1, 5));
        a.add_transition(1, actc, 1, frac!(3, 4));
        a.add_transition(2, actd, 2, frac!(1, 4));

        a.set_final_probability(2, frac!(1, 4));

        a.remove_tau_transitions().unwrap();

        // no tau labels
        for st in &a.states {
            assert!(st.transitions.iter().all(|t| !t.label.is_none()));
        }

        // expected exact result
        let mut expect = HashMap::new();
        expect.insert((0, acta, 0), frac!(3, 4));
        expect.insert((0, actb, 1), frac!(1, 4));
        expect.insert((1, actc, 1), frac!(6, 7));
        expect.insert((1, actd, 2), frac!(1, 14));
        expect.insert((2, actc, 1), frac!(3, 7));
        expect.insert((2, actd, 2), frac!(2, 7));

        let expect_final = [frac!(0, 1), frac!(1, 14), frac!(2, 7)];

        assert_eq!(collect(&a.states), expect, "visible multiset differs");
        for (i, st) in a.states.iter().enumerate() {
            assert_eq!(
                st.termination_probability, expect_final[i],
                "p_final mismatch {}",
                i
            );
        }
        assert_rows_sum_to_one(&a.states);
    }

    // 2. inter-SCC tau-edge test (row = 1)
    #[test]
    fn tau_removal_inter_scc_path() {
        // SCC_A: 0 self-loop 1/2
        // SCC_B: 1,2 (1/4,1/3)
        // cross A -> B: 0 -> 1 1/2
        let mut a = StochasticNondeterministicFiniteAutomaton::new();
        for s in 0..3 {
            a.ensure_state(s);
        }
        a.set_final_probability(0, Fraction::zero());

        a.add_transition(0, None, 0, frac!(1, 2));
        a.add_transition(0, None, 1, frac!(1, 2));
        a.add_transition(1, None, 2, frac!(1, 4));
        a.add_transition(2, None, 1, frac!(1, 3));

        let actv = Some(a.activity_key.process_activity("v"));
        let actw = Some(a.activity_key.process_activity("w"));

        // visible edges
        a.add_transition(2, actv, 2, frac!(1, 2));
        a.add_transition(1, actw, 1, frac!(1, 4)); // completes row of state 1

        // finals to complete rows
        a.set_final_probability(1, frac!(1, 2));
        a.set_final_probability(2, frac!(1, 6));

        a.remove_tau_transitions().unwrap();

        // "v" must be reachable from 0 and 1 now
        assert!(a.states[0].transitions.iter().any(|t| t.label == actv));
        assert!(a.states[1].transitions.iter().any(|t| t.label == actv));
        assert_rows_sum_to_one(&a.states);
    }

    // 3. empty automaton
    #[test]
    fn tau_removal_empty_automaton() {
        let mut a = StochasticNondeterministicFiniteAutomaton {
            states: vec![],
            initial_state: Some(0),
            activity_key: ActivityKey::new(),
        };
        a.remove_tau_transitions().unwrap(); // should not panic
        assert!(a.states.is_empty());
    }

    // 4. identity when no tau-edges
    #[test]
    fn tau_removal_no_tau_edges_is_identity() {
        let mut a = StochasticNondeterministicFiniteAutomaton::new();
        let actx = Some(a.activity_key.process_activity("x"));
        a.add_transition(0, actx, 0, frac!(2, 3));
        a.set_final_probability(0, frac!(1, 3));

        let before = collect(&a.states);
        let before_final = a.states[0].termination_probability.clone();

        a.remove_tau_transitions().unwrap();

        assert_eq!(collect(&a.states), before);
        assert_eq!(a.states[0].termination_probability, before_final);
    }

    // 5. single-state trivial case
    #[test]
    fn tau_removal_single_state_self_final() {
        let mut a = StochasticNondeterministicFiniteAutomaton::new();
        a.remove_tau_transitions().unwrap();
        assert_eq!(a.states.len(), 1);
        assert_eq!(a.states[0].termination_probability, frac!(1, 1));
    }

    // 6. simple acyclic tau-chain
    #[test]
    fn tau_removal_tau_chain() {
        // 0 -(1/5)->1 -(1/2)->2 , visible 2 -a->2
        let mut a = StochasticNondeterministicFiniteAutomaton::new();
        for s in 0..3 {
            a.ensure_state(s);
        }

        let acta = Some(a.activity_key.process_activity("a"));

        a.add_transition(0, None, 1, frac!(1, 5));
        a.add_transition(1, None, 2, frac!(1, 2));
        a.add_transition(2, acta, 2, frac!(1, 2));

        a.set_final_probability(0, frac!(4, 5));
        a.set_final_probability(1, frac!(1, 2));
        a.set_final_probability(2, frac!(1, 2));

        a.remove_tau_transitions().unwrap();

        // expect new a-edge from 0 with prob 1/20
        let w = a.states[0]
            .transitions
            .iter()
            .find(|t| t.label == acta)
            .unwrap()
            .probability
            .clone();
        assert_eq!(w, frac!(1, 20));
        assert_rows_sum_to_one(&a.states);
    }

    // 7. singular matrix must panic
    #[test]
    #[should_panic(expected = "singular")]
    fn tau_removal_singular_matrix_panics() {
        // single state with tau-self-loop weight 1  ->  I-W singular
        let mut a = StochasticNondeterministicFiniteAutomaton::new();
        a.add_transition(0, None, 0, frac!(1, 1));
        a.remove_tau_transitions().unwrap(); // must panic
    }

    // 8. automaton with only tau-edges
    #[test]
    fn tau_removal_only_tau_edges() {
        let mut a = StochasticNondeterministicFiniteAutomaton::new();
        a.ensure_state(1);
        a.add_transition(0, None, 1, frac!(1, 2));
        a.add_transition(1, None, 0, frac!(1, 2));
        a.set_final_probability(0, frac!(1, 2));
        a.set_final_probability(1, frac!(1, 2));

        a.remove_tau_transitions().unwrap();

        for st in &a.states {
            assert!(st.transitions.is_empty());
        }
        assert_rows_sum_to_one(&a.states);
    }

    // 9. size 1 / no tau-edge
    #[test]
    fn tau_removal_trivial_source_state() {
        // 0 has only visible edges, 1 has tau-edges
        let mut a = StochasticNondeterministicFiniteAutomaton::new();
        a.ensure_state(1);

        let actx = Some(a.activity_key.process_activity("x"));
        let acty = Some(a.activity_key.process_activity("y"));

        a.add_transition(0, actx, 0, frac!(1, 2));
        a.set_final_probability(0, frac!(1, 2));

        a.add_transition(1, None, 1, frac!(1, 2)); // tau self-loop
        a.add_transition(1, acty, 1, frac!(1, 2));

        a.remove_tau_transitions().unwrap();
        assert_rows_sum_to_one(&a.states);
    }
}
