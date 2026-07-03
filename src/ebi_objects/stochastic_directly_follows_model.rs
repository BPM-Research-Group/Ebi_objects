use crate::{
    Activity, ActivityKey, ActivityKeyTranslator, AutomatonSemantics, AutomatonState, EbiObject,
    Exportable, Graphable, HasActivityKey, Importable, Infoable, StochasticAutomatonSemantics,
    TranslateActivityKey, dfg_format_comparison,
    ebi_objects::labelled_petri_net::TransitionIndex,
    line_reader::LineReader,
    traits::{
        graphable,
        importable::{ImporterParameter, ImporterParameterValues, from_string},
    },
};
#[cfg(any(test, feature = "testactivities"))]
use ebi_activity_key::TestActivityKey;
use ebi_bpmn::ebi_arithmetic::anyhow::{Context, Error, Result, anyhow};
use ebi_bpmn::ebi_arithmetic::{Fraction, Signed, Zero};
use ebi_derive::ActivityKey;
use itertools::Itertools;
use layout::topo::layout::VisualGraph;
use std::{
    cmp::Ordering,
    fmt::Display,
    io::{BufRead, Write},
};

pub const HEADER: &str = "stochastic directly follows model";

#[derive(ActivityKey, Debug, Clone)]
pub struct StochasticDirectlyFollowsModel {
    pub activity_key: ActivityKey,
    pub node_2_activity: Vec<Activity>,
    pub empty_traces_weight: Fraction,
    pub sources: Vec<AutomatonState>,      //edge -> source of edge
    pub targets: Vec<AutomatonState>,      //edge -> target of edge
    pub weights: Vec<Fraction>,            //edge -> how often observed
    pub start_node_weights: Vec<Fraction>, //node -> how often observed
    pub end_node_weights: Vec<Fraction>,   //node -> how often observed
}

impl StochasticDirectlyFollowsModel {
    /**
     * Creates a new stochastic directly follows model. This has the empty stochastic language, until a state or an empty trace is added.
     */
    pub fn new(activity_key: ActivityKey) -> Self {
        Self {
            activity_key: activity_key,
            node_2_activity: vec![],
            empty_traces_weight: Fraction::zero(),
            sources: vec![],
            targets: vec![],
            weights: vec![],
            start_node_weights: vec![],
            end_node_weights: vec![],
        }
    }

    pub fn add_node(&mut self, activity: Activity) -> AutomatonState {
        let index = self.node_2_activity.len();
        self.node_2_activity.push(activity);
        self.start_node_weights.push(Fraction::zero());
        self.end_node_weights.push(Fraction::zero());
        AutomatonState::of(index)
    }

    pub fn add_empty_trace(&mut self, weight: &Fraction) {
        self.empty_traces_weight += weight;
    }

    pub fn add_edge(&mut self, source: AutomatonState, target: AutomatonState, weight: Fraction) {
        let (found, from) = self.binary_search(source, target);
        if found {
            //edge already present
            self.weights[from] += weight;
        } else {
            //new edge
            self.sources.insert(from, source);
            self.targets.insert(from, target);
            self.weights.insert(from, weight);
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
        target1: AutomatonState,
        source2: AutomatonState,
        target2: AutomatonState,
    ) -> Ordering {
        if source1 < source2 {
            return Ordering::Greater;
        } else if source1 > source2 {
            return Ordering::Less;
        } else if target2 > target1 {
            return Ordering::Greater;
        } else if target2 < target1 {
            return Ordering::Less;
        } else {
            return Ordering::Equal;
        }
    }
}

impl Importable for StochasticDirectlyFollowsModel {
    const FILE_FORMAT_SPECIFICATION_LATEX: &str = concat!("A stochastic directly follows model is a line-based structure. Lines starting with a \\# are ignored.
    This first line is exactly `directly follows model'.\\
    The second line is a boolean indicating whether the model supports empty traces.\\
    The third line is the number of activities in the model.\\
    The following lines each contain an activity. 
    For a single-line activity, escape a starting `\\$' by doubling it to `\\$'.
    For a multiline activity, start with a line `\\$multiline', and end with a line `multiline\\$'
    To end any line end with `\\$', double it to `\\$\\$'.\\
    The next line contains the number of start activities, followed by, for each start activity, a line with the index of the start activity, followed by a `w` and the weight of the start activity.\\
    The next line contains the number of end activities, followed by, for each end activity, a line with the index of the end activity, followed by a `w` and the weight of the end activity.\\
    The next line contains the number of edges, followed by, for each edge, a line with first the index of the source activity, then the `>` symbol, then the index of the target activity, then a `w`, and then the weight of the transition.
    
    For instance:
    \\lstinputlisting[language=ebilines, style=boxed]{../testfiles/aa-ab-ba.sdfm}", dfg_format_comparison!());

    const IMPORTER_PARAMETERS: &[ImporterParameter] = &[];

    fn import_as_object(
        reader: &mut dyn BufRead,
        parameter_values: &ImporterParameterValues,
    ) -> Result<EbiObject> {
        Ok(EbiObject::StochasticDirectlyFollowsModel(Self::import(
            reader,
            parameter_values,
        )?))
    }

    fn import(reader: &mut dyn BufRead, _: &ImporterParameterValues) -> Result<Self>
    where
        Self: Sized,
    {
        let mut lreader = LineReader::new(reader);

        let head = lreader
            .next_line_string()
            .with_context(|| format!("failed to read header, which should be {}", HEADER))?;
        if head != HEADER {
            return Err(anyhow!(
                "first line should be exactly `{}`, but found `{}`",
                HEADER,
                lreader.get_last_line()
            ));
        }

        //read empty traces
        let empty_traces_weight = lreader
            .next_line_weight()
            .context("could not read whether the model supports empty traces")?;

        //read nodes
        let number_of_nodes = lreader
            .next_line_index()
            .context("could not read the number of nodes")?;
        let mut activity_key = ActivityKey::new();
        let mut node_2_activity = vec![];
        for activity in 0..number_of_nodes {
            let activity = lreader
                .next_activity(&mut activity_key)
                .with_context(|| format!("Could not read node {}.", activity))?;
            node_2_activity.push(activity);
        }

        //read start nodes
        let mut start_node_weights = vec![Fraction::zero(); node_2_activity.len()];
        let number_of_start_nodes = lreader
            .next_line_index()
            .context("could not read the number of start nodes")?;
        for i in 0..number_of_start_nodes {
            let line = lreader
                .next_line_string()
                .with_context(|| format!("could not read start node {}", i))?;
            let (node, weight) = line.split('w').next_tuple().with_context(|| {
                format!("start node {} should be a node, then w, then a weight", i)
            })?;

            let node = node.parse::<usize>().with_context(|| {
                format!("start node {} should be a node, then w, then a weight", i)
            })?;
            let weight = weight.parse::<Fraction>().with_context(|| {
                format!("start node {} should be a node, then w, then a weight", i)
            })?;

            start_node_weights[node] = weight;
        }

        //read end activities
        let mut end_node_weights = vec![Fraction::zero(); node_2_activity.len()];
        let number_of_end_nodes = lreader
            .next_line_index()
            .context("could not read the number of end nodes")?;
        for i in 0..number_of_end_nodes {
            let line = lreader
                .next_line_string()
                .with_context(|| format!("could not read end node {}", i))?;
            let (node, weight) = line.split('w').next_tuple().with_context(|| {
                format!("end node {} should be a node, then w, then a weight", i)
            })?;

            let node = node.parse::<usize>().with_context(|| {
                format!("end node {} should be a node, then w, then a weight", i)
            })?;
            let weight = weight.parse::<Fraction>().with_context(|| {
                format!("end node {} should be a node, then w, then a weight", i)
            })?;

            end_node_weights[node] = weight;
        }

        let mut result = Self {
            activity_key,
            node_2_activity,
            empty_traces_weight,
            sources: vec![],
            targets: vec![],
            weights: vec![],
            start_node_weights,
            end_node_weights,
        };

        //read edges
        let number_of_edges = lreader
            .next_line_index()
            .context("could not read number of edges")?;
        for e in 0..number_of_edges {
            let edge_line = lreader
                .next_line_string()
                .with_context(|| format!("could not read edge {}", e))?;

            let mut arr = edge_line.split('>');
            if let Some((source, remainder)) = arr.next_tuple() {
                let source = AutomatonState::of(
                    source
                        .parse::<usize>()
                        .with_context(|| format!("could not read source of edge {}", e))?,
                );

                let mut arr = remainder.split('w');
                if let Some((target, weight)) = arr.next_tuple() {
                    let target = AutomatonState::of(
                        target
                            .parse::<usize>()
                            .with_context(|| format!("could not read target of edge {}", e))?,
                    );

                    let weight = weight
                        .parse::<Fraction>()
                        .with_context(|| format!("could not read weight of edge {}", e))?;

                    result.add_edge(source, target, weight);
                } else {
                    return Err(anyhow!(
                        "could not read edge, which must be two numbers separated by >, followed by w and a weight"
                    ));
                }
            } else {
                return Err(anyhow!(
                    "could not read edge, which must be two numbers separated by >, followed by w and a weight"
                ));
            }
        }

        Ok(result)
    }
}
from_string!(StochasticDirectlyFollowsModel);

impl Exportable for StochasticDirectlyFollowsModel {
    fn export_from_object(object: EbiObject, f: &mut dyn Write) -> Result<()> {
        match object {
            EbiObject::StochasticDirectlyFollowsModel(dfm) => Self::export(&dfm, f),
            EbiObject::DirectlyFollowsGraph(dfm) => {
                Self::export(&Into::<StochasticDirectlyFollowsModel>::into(dfm), f)
            }
            _ => Err(anyhow!(
                "cannot export {} {} as an SDFM",
                object.get_type().get_article(),
                object.get_type()
            )),
        }
    }

    fn export(&self, f: &mut dyn std::io::Write) -> Result<()> {
        Ok(write!(f, "{}", self.to_string())?)
    }
}

impl TranslateActivityKey for StochasticDirectlyFollowsModel {
    fn translate_using_activity_key(&mut self, to_activity_key: &mut ActivityKey) {
        let translator = ActivityKeyTranslator::new(&self.activity_key, to_activity_key);
        self.node_2_activity
            .iter_mut()
            .for_each(|activity| *activity = translator.translate_activity(&activity));
        self.activity_key = to_activity_key.clone();
    }
}

impl Display for StochasticDirectlyFollowsModel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}", HEADER)?;

        writeln!(f, "# empty traces weight\n{}", self.empty_traces_weight)?;

        //activities
        writeln!(f, "# number of nodes\n{}", self.node_2_activity.len())?;
        for (a, activity) in self.node_2_activity.iter().enumerate() {
            writeln!(f, "#node {}", a,)?;
            LineReader::write_multiline_activity(f, activity, &self.activity_key)?;
        }

        //start activities
        writeln!(
            f,
            "# number of start nodes\n{}",
            self.start_node_weights
                .iter()
                .filter(|x| x.is_positive())
                .count()
        )?;
        for (i, weight) in self.start_node_weights.iter().enumerate() {
            if weight.is_positive() {
                writeln!(f, "{}w{}", i, weight)?;
            }
        }

        //end activities
        writeln!(
            f,
            "# number of end nodes\n{}",
            self.end_node_weights
                .iter()
                .filter(|x| x.is_positive())
                .count()
        )?;
        for (i, weight) in self.end_node_weights.iter().enumerate() {
            if weight.is_positive() {
                writeln!(f, "{}w{}", i, weight)?;
            }
        }

        //edges
        writeln!(f, "# number of edges\n{}\n# edges", self.sources.len())?;
        for (source, (target, weight)) in self
            .sources
            .iter()
            .zip(self.targets.iter().zip(self.weights.iter()))
        {
            writeln!(f, "{}>{}w{}", source, target, weight)?;
        }

        Ok(write!(f, "")?)
    }
}

impl Graphable for StochasticDirectlyFollowsModel {
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
        let mut nodes = vec![];
        for node in &self.node_2_activity {
            nodes.push(graphable::create_transition(
                &mut graph,
                self.activity_key.get_activity_label(node),
                "",
            ));
        }

        //start activities
        for (activity, weight) in self.start_node_weights.iter().enumerate() {
            if weight.is_positive() {
                graphable::create_edge(
                    &mut graph,
                    &source,
                    &nodes[activity],
                    &format!("{}", weight),
                );
            }
        }

        //end activities
        for (activity, weight) in self.end_node_weights.iter().enumerate() {
            if weight.is_positive() {
                graphable::create_edge(&mut graph, &nodes[activity], &sink, &format!("{}", weight));
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
                &nodes[*source],
                &nodes[*target],
                &format!("{}", weight),
            );
        }

        Ok(graph)
    }
}

impl Infoable for StochasticDirectlyFollowsModel {
    fn info(&self, f: &mut impl std::io::Write) -> Result<()> {
        writeln!(f, "Number of transitions\t{}", self.node_2_activity.len())?;
        writeln!(
            f,
            "Number of activities\t{}",
            self.activity_key.activity2name.len()
        )?;
        writeln!(f, "Number of nodes\t\t{}", self.node_2_activity.len())?;
        writeln!(f, "Number of edges\t\t{}", self.sources.len())?;
        writeln!(
            f,
            "Number of start nodes\t{}",
            self.start_node_weights
                .iter()
                .filter(|x| x.is_positive())
                .count()
        )?;
        writeln!(
            f,
            "Number of end nodes\t{}",
            self.end_node_weights
                .iter()
                .filter(|x| x.is_positive())
                .count()
        )?;

        let mut sum: Fraction = self.weights.iter().sum();
        sum += &self.start_node_weights.iter().sum::<Fraction>();
        sum += &self.end_node_weights.iter().sum::<Fraction>();
        writeln!(f, "Sum weight of edges\t{}", sum)?;

        writeln!(f, "")?;
        self.activity_key().info(f)?;

        Ok(writeln!(f, "")?)
    }
}

/**
 * States:
 * Initial state = nodes[len]
 * Final state = nodes[len] + 1. A final state is reached after executing a silent step from an end activity node.
 *
 * Transitions:
 * End activity -> final state = edges[len]
 * initial state -> start activity = edges[len] + 1 ...
 */
impl AutomatonSemantics for StochasticDirectlyFollowsModel {
    fn initial_state(&self) -> Option<AutomatonState> {
        if self.node_2_activity.is_empty() && !self.empty_traces_weight.is_positive() {
            None
        } else {
            Some(AutomatonState::of(self.node_2_activity.len()))
        }
    }

    fn number_of_states(&self) -> usize {
        self.node_2_activity.len() + 2
    }

    fn states(&self) -> impl Iterator<Item = AutomatonState> {
        (0..self.number_of_states()).map(|x| AutomatonState::of(x))
    }

    fn is_state_final(&self, state: AutomatonState) -> bool {
        state.0 == self.node_2_activity.len() + 1
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
                    Some(self.node_2_activity[*target]),
                )
            });

        //end transitions
        let end_transition = self.sources.len();
        let end_state = AutomatonState::of(self.node_2_activity.len() + 1);
        let it2 = self
            .end_node_weights
            .iter()
            .enumerate()
            .filter(|(_, weight)| weight.is_positive())
            .map(move |(state, _)| (end_transition, AutomatonState::of(state), end_state, None));

        //start transitions
        let start_transition = self.sources.len() + 1;
        let start_state = AutomatonState::of(self.node_2_activity.len());
        let it3 = self
            .end_node_weights
            .iter()
            .enumerate()
            .filter(|(_, weight)| weight.is_positive())
            .map(move |(state, _)| {
                (
                    start_transition,
                    start_state,
                    AutomatonState::of(state),
                    Some(self.node_2_activity[state]),
                )
            });

        it1.chain(it2).chain(it3)
    }

    fn outgoing_transitions(&self, state: AutomatonState) -> Vec<TransitionIndex> {
        if state.0 == self.node_2_activity.len() {
            //we are in the initial state
            let mut result = self
                .start_node_weights
                .iter()
                .enumerate()
                .filter_map(|(node, w)| {
                    if w.is_positive() {
                        Some(self.sources.len() + 1 + node)
                    } else {
                        None
                    }
                })
                .collect::<Vec<_>>();

            if self.empty_traces_weight.is_positive() {
                result.push(self.sources.len())
            }

            result
        } else if state.0 > self.node_2_activity.len() {
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
            if self.end_node_weights[state].is_positive() {
                result.push(self.sources.len())
            }

            result
        }
    }

    fn transition_2_target(&self, transition: TransitionIndex) -> Option<AutomatonState> {
        if transition == self.sources.len() {
            //end / empty
            Some(AutomatonState::of(self.node_2_activity.len() + 1))
        } else if transition < self.sources.len() {
            //edge
            Some(AutomatonState::of(self.targets[transition].0))
        } else if transition < self.sources.len() + 1 + self.node_2_activity.len() {
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
            Some(self.node_2_activity[self.targets[node].0])
        } else if transition < self.sources.len() + 1 + self.node_2_activity.len() {
            //start
            let node = transition - (self.sources.len() + 1);
            Some(self.node_2_activity[node])
        } else {
            None
        }
    }

    fn is_transition_silent(&self, transition: TransitionIndex) -> bool {
        transition == self.sources.len()
    }
}

impl StochasticAutomatonSemantics for StochasticDirectlyFollowsModel {
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
            //terminating transition
            self.end_node_weights.get(state.0)
        } else if transition > self.sources.len() {
            //start transition
            self.start_node_weights.get(transition - self.sources.len() - 1)
        } else {
            self.weights.get(transition)
        }
    }
}

#[cfg(any(test, feature = "testactivities"))]
impl TestActivityKey for StochasticDirectlyFollowsModel {
    fn test_activity_key(&self) {
        self.node_2_activity
            .iter()
            .for_each(|activity| self.activity_key().assert_activity_is_of_key(activity));
    }
}
