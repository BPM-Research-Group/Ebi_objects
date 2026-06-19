use crate::{
    Activity, ActivityKey, ActivityKeyTranslator, AutomatonSemantics, AutomatonState, Graphable,
    HasActivityKey, Infoable, StochasticAutomatonSemantics, TranslateActivityKey,
    constants::ebi_object::EbiObject,
    dfg_format_comparison,
    ebi_objects::labelled_petri_net::TransitionIndex,
    json,
    traits::{
        exportable::Exportable,
        graphable,
        importable::{Importable, ImporterParameter, ImporterParameterValues, from_string},
    },
};
#[cfg(any(test, feature = "testactivities"))]
use ebi_activity_key::TestActivityKey;
use ebi_bpmn::ebi_arithmetic::{
    Fraction, Signed, Zero,
    anyhow::{Context, Error, Result, anyhow},
};
use ebi_derive::ActivityKey;
use intmap::{Entry, IntMap};
use layout::topo::layout::VisualGraph;
use serde_json::Value;
use std::{
    cmp::Ordering,
    collections::{HashMap, hash_map},
    fmt::Display,
};

/// A directly follows graph: a graph of weighted transitions (normalised on-the-fly), supports empty traces, and explicit start and end activities.
/// The fields are public, however for convenience of access, the traits `AutomatonSemantics` and `StochasticAutomatonSemantics` are recommended.
#[derive(ActivityKey, Clone, Debug)]
pub struct DirectlyFollowsGraph {
    pub activity_key: ActivityKey,
    pub state_2_activity: Vec<Activity>, //invariant: each activity appears only once
    pub activity_2_state: IntMap<Activity, AutomatonState>, //invariant: each activity appears only once
    pub empty_traces_weight: Fraction,
    pub sources: Vec<AutomatonState>, //edge -> source of edge
    pub targets: Vec<AutomatonState>, //edge -> target of edge
    pub weights: Vec<Fraction>,       //edge -> how often observed
    pub start_states: IntMap<AutomatonState, Fraction>, //state -> how often observed
    pub end_states: IntMap<AutomatonState, Fraction>, //state -> how often observed
}

impl DirectlyFollowsGraph {
    pub fn new(activity_key: ActivityKey) -> Self {
        Self {
            empty_traces_weight: Fraction::zero(),
            activity_key: activity_key,
            state_2_activity: vec![],
            activity_2_state: IntMap::new(),
            sources: vec![],
            targets: vec![],
            weights: vec![],
            start_states: IntMap::new(),
            end_states: IntMap::new(),
        }
    }

    pub fn edge_weight(&self, source: AutomatonState, target: AutomatonState) -> Option<&Fraction> {
        let (found, from) = self.binary_search(source, target);
        if found {
            Some(&self.weights[from])
        } else {
            None
        }
    }

    pub fn edge_weight_activities(&self, source: Activity, target: Activity) -> Fraction {
        if let (Some(source), Some(target)) = (
            self.activity_2_state.get(source),
            self.activity_2_state.get(target),
        ) {
            let (found, from) = self.binary_search(*source, *target);
            if found {
                self.weights[from].clone()
            } else {
                Fraction::zero()
            }
        } else {
            Fraction::zero()
        }
    }

    pub fn is_start_activity(&self, activity: Activity) -> bool {
        if let Some(state) = self.activity_2_state.get(activity) {
            if let Some(weight) = self.start_states.get(*state) {
                weight.is_positive()
            } else {
                false
            }
        } else {
            false
        }
    }

    pub fn is_end_activity(&self, activity: Activity) -> bool {
        if let Some(state) = self.activity_2_state.get(activity) {
            if let Some(weight) = self.end_states.get(*state) {
                weight.is_positive()
            } else {
                false
            }
        } else {
            false
        }
    }

    pub fn start_activities(&self) -> impl Iterator<Item = Activity> {
        self.start_states.iter().filter_map(|(state, weight)| {
            if weight.is_positive() {
                Some(self.state_2_activity[state])
            } else {
                None
            }
        })
    }

    pub fn end_activities(&self) -> impl Iterator<Item = Activity> {
        self.end_states.iter().filter_map(|(state, weight)| {
            if weight.is_positive() {
                Some(self.state_2_activity[state])
            } else {
                None
            }
        })
    }

    pub fn start_activity_weight(&self, activity: Activity) -> Fraction {
        if let Some(node) = self.activity_2_state.get(activity) {
            self.start_states
                .get(*node)
                .cloned()
                .unwrap_or_else(|| Fraction::zero())
        } else {
            Fraction::zero()
        }
    }

    pub fn end_activity_weight(&self, activity: Activity) -> Fraction {
        if let Some(node) = self.activity_2_state.get(activity) {
            self.end_states
                .get(*node)
                .cloned()
                .unwrap_or_else(|| Fraction::zero())
        } else {
            Fraction::zero()
        }
    }

    pub fn is_start_node(&self, activity: Activity) -> bool {
        if let Some(node) = self.activity_2_state.get(activity) {
            match self.start_states.get(*node) {
                Some(w) => w.is_positive(),
                None => false,
            }
        } else {
            false
        }
    }

    pub fn is_end_node(&self, activity: Activity) -> bool {
        if let Some(node) = self.activity_2_state.get(activity) {
            match self.end_states.get(*node) {
                Some(w) => w.is_positive(),
                None => false,
            }
        } else {
            false
        }
    }

    pub fn activity_cardinality(&self, activity: Activity) -> Fraction {
        if let Some(node) = self.activity_2_state.get(activity) {
            let mut result = match self.end_states.get(*node) {
                Some(a) => a.clone(),
                None => Fraction::zero(),
            };

            let (_, mut i) = self.binary_search(*node, AutomatonState::zero());
            while i < self.sources.len() && &self.sources[i] == node {
                if self.weights[i].is_positive() {
                    result += &self.weights[i];
                }
                i += 1;
            }

            result
        } else {
            Fraction::zero()
        }
    }

    pub fn has_empty_traces(&self) -> bool {
        self.empty_traces_weight.is_positive()
    }

    pub fn get_sources(&self) -> impl Iterator<Item = Activity> {
        self.sources
            .iter()
            .map(|state| self.state_2_activity[state])
    }

    pub fn get_targets(&self) -> impl Iterator<Item = Activity> {
        self.targets
            .iter()
            .map(|state| self.state_2_activity[state])
    }

    fn add_or_get_state(&mut self, activity: Activity) -> AutomatonState {
        let new_node = AutomatonState::of(self.activity_2_state.len());
        match self.activity_2_state.entry(activity) {
            intmap::Entry::Occupied(occupied_entry) => *occupied_entry.get(),
            intmap::Entry::Vacant(vacant_entry) => {
                self.state_2_activity.push(activity);
                vacant_entry.insert(new_node);
                new_node
            }
        }
    }

    pub fn add_empty_trace(&mut self, weight: &Fraction) {
        self.empty_traces_weight += weight;
    }

    pub fn add_start_activity(&mut self, activity: Activity, weight: &Fraction) {
        let node = self.add_or_get_state(activity);
        match self.start_states.entry(node) {
            Entry::Occupied(mut occupied_entry) => *occupied_entry.get_mut() += weight,
            Entry::Vacant(vacant_entry) => {
                vacant_entry.insert(weight.clone());
            }
        }
    }

    pub fn add_end_activity(&mut self, activity: Activity, weight: &Fraction) {
        let node = self.add_or_get_state(activity);
        match self.end_states.entry(node) {
            Entry::Occupied(mut occupied_entry) => *occupied_entry.get_mut() += weight,
            Entry::Vacant(vacant_entry) => {
                vacant_entry.insert(weight.clone());
            }
        }
    }

    pub fn remove_start_activity(&mut self, activity: Activity) {
        let node = self.add_or_get_state(activity);
        self.start_states.remove(node);
    }

    pub fn remove_end_activity(&mut self, activity: Activity) {
        let node = self.add_or_get_state(activity);
        self.end_states.remove(node);
    }

    pub fn outgoing_edges(&mut self, source: Activity) -> Vec<(Activity, &Fraction)> {
        let mut result = vec![];
        if let Some(source_state) = self.activity_2_state.get(source) {
            //add edges
            let (_, mut i) = self.binary_search(*source_state, AutomatonState::zero());
            while i < self.sources.len() && self.sources[i] == *source_state {
                if self.weights[i].is_positive() {
                    result.push((self.state_2_activity[i], &self.weights[i]));
                }
                i += 1;
            }

            result
        } else {
            vec![]
        }
    }

    pub fn add_edge(&mut self, source: Activity, target: Activity, weight: &Fraction) {
        let source = self.add_or_get_state(source);
        let target = self.add_or_get_state(target);
        let (found, from) = self.binary_search(source, target);
        if found {
            //edge already present
            self.weights[from] += weight;
        } else {
            //new edge
            self.sources.insert(from, source);
            self.targets.insert(from, target);
            self.weights.insert(from, weight.clone());
        }
    }

    pub fn remove_edge(&mut self, source: Activity, target: Activity) {
        let source = self.add_or_get_state(source);
        let target = self.add_or_get_state(target);
        let (found, from) = self.binary_search(source, target);
        if found {
            //edge already present
            self.sources.remove(from);
            self.targets.remove(from);
            self.weights.remove(from);
        }
    }

    pub fn binary_search(&self, source: AutomatonState, target: AutomatonState) -> (bool, usize) {
        if self.sources.is_empty() {
            return (false, 0);
        }

        let mut size = self.sources.len();
        let mut left = 0;
        let mut right = size;
        while left < right {
            let mid = left + size / 2;

            let cmp = Self::compare(source, target, self.sources[mid], self.targets[mid]);

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

    fn compare(
        source1: AutomatonState,
        activity1: AutomatonState,
        source2: AutomatonState,
        activity2: AutomatonState,
    ) -> Ordering {
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
}

impl Importable for DirectlyFollowsGraph {
    const FILE_FORMAT_SPECIFICATION_LATEX: &str = concat!(
        "A directly follows graph is a JSON structure.
    
    For instance:
    \\lstinputlisting[language=ebilines, style=boxed]{../testfiles/aa-ab-ba.dfg}",
        dfg_format_comparison!()
    );

    const IMPORTER_PARAMETERS: &[ImporterParameter] = &[];

    fn import_as_object(
        reader: &mut dyn std::io::BufRead,
        parameter_values: &ImporterParameterValues,
    ) -> Result<EbiObject> {
        Ok(EbiObject::DirectlyFollowsGraph(Self::import(
            reader,
            parameter_values,
        )?))
    }

    fn import(reader: &mut dyn std::io::BufRead, _: &ImporterParameterValues) -> Result<Self>
    where
        Self: Sized,
    {
        let json: Value = serde_json::from_reader(reader)?;

        let mut result = DirectlyFollowsGraph::new(ActivityKey::new());

        let activities = json::read_field_object(&json, "activities")
            .with_context(|| format!("reading field `activities`"))?;
        let frequencies: Result<HashMap<Activity, Fraction>> = activities
            .into_iter()
            .map(|(activity, json)| {
                let frequency = json::read_fraction(json)
                    .with_context(|| format!("read activity frequency of {}", activity))?;
                let activity = result.activity_key.process_activity(activity);
                Ok((activity, frequency))
            })
            .collect();
        let mut frequencies_in = frequencies?;
        let mut frequencies_out = frequencies_in.clone();

        let edges = json::read_field_list(&json, "directly_follows_relations")
            .with_context(|| format!("reading field `directly follows relations`"))?;
        for (i, edge) in edges.into_iter().enumerate() {
            let edge_list = json::read_list(edge).with_context(|| format!("reading edge {}", i))?;
            let sourcetarget = json::read_list(
                edge_list
                    .get(0)
                    .ok_or_else(|| anyhow!("could not read source and target of edge {}", i))?,
            )?;

            let source_act = json::read_string(
                sourcetarget
                    .get(0)
                    .ok_or_else(|| anyhow!("could not read source of edge {}", i))?,
            )
            .with_context(|| format!("reading source of edge {}", i))?;
            let source = result.activity_key.process_activity(source_act);

            let target_act = json::read_string(
                sourcetarget
                    .get(1)
                    .ok_or_else(|| anyhow!("could not read target of edge {}", i))?,
            )
            .with_context(|| format!("reading target of edge {}", i))?;
            let target = result.activity_key.process_activity(target_act);

            let weight = json::read_fraction(
                edge_list
                    .get(1)
                    .ok_or_else(|| anyhow!("could not read weight of edge {}", i))?,
            )
            .with_context(|| format!("reading weight of edge {}", i))?;

            result.add_edge(source, target, &weight);

            //keep track of how many executions are left for source
            match frequencies_out.entry(source) {
                hash_map::Entry::Occupied(mut occupied_entry) => {
                    let pre_weight = occupied_entry.get();
                    if pre_weight < &weight {
                        return Err(anyhow!(
                            "activity {} has too many outgoing edges as of edge {}",
                            source_act,
                            i
                        ));
                    } else {
                        *occupied_entry.get_mut() -= &weight
                    }
                }
                hash_map::Entry::Vacant(_) => {
                    //activity not declared
                    return Err(anyhow!(
                        "non-declared activity {} used as source of edge {}",
                        source_act,
                        i
                    ));
                }
            }

            //keep track of how many executions are left for source
            match frequencies_in.entry(target) {
                hash_map::Entry::Occupied(mut occupied_entry) => {
                    let pre_weight = occupied_entry.get();
                    if pre_weight < &weight {
                        return Err(anyhow!(
                            "activity {} has too many incoming edges as of edge {}",
                            target_act,
                            i
                        ));
                    } else {
                        *occupied_entry.get_mut() -= &weight
                    }
                }
                hash_map::Entry::Vacant(_) => {
                    //activity not declared
                    return Err(anyhow!(
                        "non-declared activity {} used as target of edge {}",
                        target_act,
                        i
                    ));
                }
            }
        }

        let start_activities = json::read_field_list(&json, "start_activities")
            .with_context(|| format!("reading field `start activities`"))?;
        let err: Result<()> = start_activities
            .into_iter()
            .map(|json| {
                let act = json::read_string(json)?;
                let activity = result.activity_key.process_activity(act);

                match frequencies_in.entry(activity) {
                    hash_map::Entry::Occupied(occupied_entry) => {
                        let weight = occupied_entry.remove();
                        if weight.is_positive() {
                            result.add_start_activity(activity, &weight);
                        }
                    }
                    hash_map::Entry::Vacant(_) => {
                        //activity not declared
                        return Err(anyhow!(format!(
                            "non-declared activity {} used as start activity",
                            act
                        )));
                    }
                }
                Ok(())
            })
            .collect();
        err?;

        let end_activities = json::read_field_list(&json, "end_activities")
            .with_context(|| format!("reading field `end activities`"))?;
        let err: Result<()> = end_activities
            .into_iter()
            .map(|json| {
                let act = json::read_string(json)?;
                let activity = result.activity_key.process_activity(act);

                match frequencies_out.entry(activity) {
                    hash_map::Entry::Occupied(occupied_entry) => {
                        let weight = occupied_entry.remove();
                        if weight.is_positive() {
                            result.add_end_activity(activity, &weight);
                        }
                    }
                    hash_map::Entry::Vacant(_) => {
                        //activity not declared
                        return Err(anyhow!(format!(
                            "non-declared activity {} used as end activity",
                            act
                        )));
                    }
                }
                Ok(())
            })
            .collect();
        err?;

        return Ok(result);
    }
}
from_string!(DirectlyFollowsGraph);

impl Exportable for DirectlyFollowsGraph {
    fn export_from_object(object: EbiObject, f: &mut dyn std::io::Write) -> Result<()> {
        match object {
            EbiObject::DirectlyFollowsGraph(dfa) => dfa.export(f),
            _ => Err(anyhow!("Cannot export to DFG.")),
        }
    }

    fn export(&self, f: &mut dyn std::io::Write) -> Result<()> {
        Ok(write!(f, "{}", self)?)
    }
}

impl TranslateActivityKey for DirectlyFollowsGraph {
    fn translate_using_activity_key(&mut self, to_activity_key: &mut ActivityKey) {
        let translator = ActivityKeyTranslator::new(&self.activity_key, to_activity_key);

        let mut new_node_2_activity = Vec::with_capacity(self.state_2_activity.len());
        let mut new_activity_2_node = IntMap::with_capacity(self.state_2_activity.len());

        for old_activity in self.state_2_activity.iter() {
            let new_activity = translator.translate_activity(&old_activity);
            let new_state = AutomatonState::of(new_node_2_activity.len());
            new_node_2_activity.push(new_activity);
            new_activity_2_node.insert(new_activity, new_state);
        }

        std::mem::swap(&mut self.state_2_activity, &mut new_node_2_activity);
        std::mem::swap(&mut self.activity_2_state, &mut new_activity_2_node);

        self.activity_key = to_activity_key.clone();
    }
}

impl Infoable for DirectlyFollowsGraph {
    fn info(&self, f: &mut impl std::io::Write) -> Result<()> {
        writeln!(f, "Number of edges\t\t\t{}", self.sources.len())?;
        writeln!(
            f,
            "Number of activities\t\t{}",
            self.activity_key.activity2name.len()
        )?;
        writeln!(f, "Number of start activities\t{}", self.start_states.len())?;
        writeln!(f, "Number of end activities\t{}", self.end_states.len())?;

        let mut sum: Fraction = self.weights.iter().sum();
        sum += &self.start_states.values().sum::<Fraction>();
        sum += &self.end_states.values().sum::<Fraction>();
        writeln!(f, "Sum weight of edges\t\t{}", sum)?;

        writeln!(f, "")?;
        self.activity_key().info(f)?;

        Ok(writeln!(f, "")?)
    }
}

impl Display for DirectlyFollowsGraph {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let activities = Value::Object(
            self.activity_key
                .get_activities()
                .iter()
                .filter_map(|activity| {
                    let cardinality = self.activity_cardinality(**activity);
                    if cardinality.is_positive() {
                        Some((
                            self.activity_key.get_activity_label(activity).to_string(),
                            { Value::String(cardinality.to_string()) },
                        ))
                    } else {
                        None
                    }
                })
                .collect(),
        );
        let start_activities = Value::Array(
            self.start_states
                .iter()
                .filter_map(|(a, w)| {
                    if w.is_positive() {
                        Some(Value::String(
                            self.activity_key
                                .get_activity_label(&self.state_2_activity[a.0])
                                .to_string(),
                        ))
                    } else {
                        None
                    }
                })
                .collect(),
        );
        let end_activities = Value::Array(
            self.end_states
                .iter()
                .filter_map(|(a, w)| {
                    if w.is_positive() {
                        Some(Value::String(
                            self.activity_key
                                .get_activity_label(&self.state_2_activity[a.0])
                                .to_string(),
                        ))
                    } else {
                        None
                    }
                })
                .collect(),
        );
        let edges = Value::Array(
            self.sources
                .iter()
                .zip(self.targets.iter().zip(self.weights.iter()))
                .map(|(source, (target, weight))| {
                    Value::Array(vec![
                        Value::Array(vec![
                            Value::String(
                                self.activity_key
                                    .get_activity_label(&self.state_2_activity[source.0])
                                    .to_string(),
                            ),
                            Value::String(
                                self.activity_key
                                    .get_activity_label(&self.state_2_activity[target.0])
                                    .to_string(),
                            ),
                        ]),
                        Value::String(weight.to_string()),
                    ])
                })
                .collect(),
        );
        let json = serde_json::json!(
            {
                "activities": activities,
                "start_activities": start_activities,
                "end_activities": end_activities,
                "directly_follows_relations": edges
            }
        );
        let x = serde_json::to_string(&json).unwrap();
        write!(f, "{}", x)
    }
}

impl Graphable for DirectlyFollowsGraph {
    fn to_dot(&self) -> Result<layout::topo::layout::VisualGraph> {
        let mut graph = VisualGraph::new(layout::core::base::Orientation::LeftToRight);

        //source + sink
        let source = graphable::create_place(&mut graph, "");
        let sink = graphable::create_place(&mut graph, "");

        //empty traces
        if self.empty_traces_weight.is_positive() {
            graphable::create_edge(
                &mut graph,
                &source,
                &sink,
                &format!("{}", self.empty_traces_weight),
            );
        }

        //nodes
        let mut nodes = vec![sink; self.activity_key.get_number_of_activities()];
        for activity in &self.activity_key.get_activities() {
            let id = self.activity_key.get_id_from_activity(*activity);
            let cardinality = self.activity_cardinality(**activity);
            if cardinality.is_positive() {
                nodes[id] = graphable::create_transition(
                    &mut graph,
                    self.activity_key.get_activity_label(activity),
                    "",
                );
            }
        }

        //start activities
        for (activity, weight) in self.start_states.iter() {
            if weight.is_positive() {
                graphable::create_edge(
                    &mut graph,
                    &source,
                    &nodes[self
                        .activity_key
                        .get_id_from_activity(&self.state_2_activity[activity.0])],
                    &format!("{}", weight),
                );
            }
        }

        //end activities
        for (activity, weight) in self.end_states.iter() {
            if weight.is_positive() {
                graphable::create_edge(
                    &mut graph,
                    &nodes[self
                        .activity_key
                        .get_id_from_activity(&self.state_2_activity[activity.0])],
                    &sink,
                    &format!("{}", weight),
                );
            }
        }

        //edges
        for (source, (target, weight)) in self
            .sources
            .iter()
            .zip(self.targets.iter().zip(self.weights.iter()))
        {
            graphable::create_edge(
                &mut graph,
                &nodes[self
                    .activity_key
                    .get_id_from_activity(&self.state_2_activity[source.0])],
                &nodes[self
                    .activity_key
                    .get_id_from_activity(&self.state_2_activity[target.0])],
                &format!("{}", weight),
            );
        }

        Ok(graph)
    }
}

/**
 * States:
 * Initial state = nodes[len]
 * Final state = nodes[len] + 1. A final state is reached after executing a silent step from an end activity node.
 *
 * Transitions:
 * End activity/Initial state -> final state = edges[len]
 * initial state -> start activity = edges[len] + 1 ...
 */
impl AutomatonSemantics for DirectlyFollowsGraph {
    fn initial_state(&self) -> Option<AutomatonState> {
        if !self.has_empty_traces() && !self.start_states.iter().any(|(_, w)| w.is_positive()) {
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
            .end_states
            .iter()
            .filter(|(_, weight)| weight.is_positive())
            .map(move |(state, _)| (end_transition, state, end_state, None));

        //start transitions
        let start_transition = self.sources.len() + 1;
        let start_state = AutomatonState::of(self.state_2_activity.len());
        let it3 = self
            .start_states
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
                .start_states
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

impl StochasticAutomatonSemantics for DirectlyFollowsGraph {
    fn outgoing_transitions_weight_sum(&self, state: AutomatonState) -> Fraction {
        self.outgoing_transitions(state)
            .into_iter()
            .filter_map(|transition| self.transition_2_weight(state, transition))
            .sum()
    }

    fn transition_2_weight(
        &self,
        state: AutomatonState,
        transition: TransitionIndex,
    ) -> Option<&Fraction> {
        if transition == self.sources.len() {
            //end
            self.end_states.get(state)
        } else if transition < self.sources.len() {
            //edge
            let node = transition;
            self.weights.get(node)
        } else if transition < self.sources.len() + 1 + self.state_2_activity.len() {
            //start
            let node = transition - (self.sources.len() + 1);
            self.start_states.get(AutomatonState::of(node))
        } else {
            None
        }
    }
}

#[cfg(any(test, feature = "testactivities"))]
impl TestActivityKey for DirectlyFollowsGraph {
    fn test_activity_key(&self) {
        self.state_2_activity
            .iter()
            .for_each(|activity| self.activity_key().assert_activity_is_of_key(activity));
    }
}
