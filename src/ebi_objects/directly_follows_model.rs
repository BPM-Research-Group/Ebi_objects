use crate::{
    Activity, ActivityKey, ActivityKeyTranslator, AutomatonSemantics, AutomatonState, Exportable,
    Graphable, HasActivityKey, Importable, Infoable, TranslateActivityKey,
    constants::ebi_object::EbiObject,
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
use ebi_derive::ActivityKey;
use itertools::Itertools;
use layout::topo::layout::VisualGraph;
use std::{cmp::Ordering, fmt::Display, io::Write};

pub const HEADER: &str = "directly follows model";

#[macro_export]
macro_rules! dfg_format_comparison {
    () => {"
    
    The following table gives an overview of several directly follows-based file types and their features:
    \\begin{center}
    \\begin{tabularx}{\\linewidth}{Xllll}
        \\toprule
        \\rotatebox{90}{File type} & \\rotatebox{90}{stochastic} & \\rotatebox{90}{multiple nodes with the same label} & \\rotatebox{90}{silent transitions} & file syntax \\\\
        \\midrule
        \\hyperref[filehandler:directly follows model]{directly follows model (.dfm)} & no & yes & no & line-based \\\\
        \\hyperref[filehandler:directly follows graph]{directly follows graph (.dfg)} & weights & no & no & JSON \\\\
        \\hyperref[filehandler:stochastic directly follows model]{stochastic directly follows model (.sdfm)} & yes & yes & no & line-based \\\\
        \\hyperref[filehandler:stochastic deterministic finite automaton]{stochastic deterministic finite automaton (.sdfa)} & yes & yes & no & line-based \\\\
        \\hyperref[filehandler:stochastic non-deterministic finite automaton]{stochastic non-deterministic finite automaton (.snfa)} & yes & yes & yes & line-based \\\\
        \\bottomrule
    \\end{tabularx}
    \\end{center}"}
}

#[derive(ActivityKey, Debug, Clone)]
pub struct DirectlyFollowsModel {
    pub activity_key: ActivityKey,
    pub node_2_activity: Vec<Activity>,
    pub empty_traces: bool,
    pub sources: Vec<AutomatonState>, //edge -> source of edge
    pub targets: Vec<AutomatonState>, //edge -> target of edge
    pub start_nodes: Vec<bool>,       //node -> how often observed
    pub end_nodes: Vec<bool>,         //node -> how often observed
}

impl DirectlyFollowsModel {
    /**
     * Creates a new directly follows model without any states or edges. This has the empty language, until a state or an empty trace is added.
     */
    pub fn new(activity_key: ActivityKey) -> Self {
        Self {
            activity_key: activity_key,
            node_2_activity: vec![],
            empty_traces: false,
            sources: vec![],
            targets: vec![],
            start_nodes: vec![],
            end_nodes: vec![],
        }
    }

    pub fn add_empty_trace(&mut self) {
        self.empty_traces = true;
    }

    pub fn add_node(&mut self, activity: Activity) -> AutomatonState {
        let index = self.node_2_activity.len();
        self.node_2_activity.push(activity);
        self.start_nodes.push(false);
        self.end_nodes.push(false);
        AutomatonState::of(index)
    }

    pub fn add_edge(&mut self, source: AutomatonState, target: AutomatonState) {
        let (found, from) = self.binary_search(source, target);
        if found {
            //edge already present
        } else {
            //new edge
            self.sources.insert(from, source);
            self.targets.insert(from, target);
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

impl TranslateActivityKey for DirectlyFollowsModel {
    fn translate_using_activity_key(&mut self, to_activity_key: &mut ActivityKey) {
        let translator = ActivityKeyTranslator::new(&self.activity_key, to_activity_key);
        self.node_2_activity
            .iter_mut()
            .for_each(|activity| *activity = translator.translate_activity(&activity));
        self.activity_key = to_activity_key.clone();
    }
}

impl Importable for DirectlyFollowsModel {
    const FILE_FORMAT_SPECIFICATION_LATEX: &str = concat!("A directly follows model is a line-based structure. Lines starting with a \\# are ignored.
    This first line is exactly `directly follows model'.\\\\
    The second line is a boolean indicating whether the model supports empty traces.\\\\
    The third line is the number of activities in the model.\\\\
    For a single-line activity, escape a starting `\\$' by doubling it to `\\$'.
    For a multiline activity, start with a line `\\$multiline', and end with a line `multiline\\$'
    To end any line end with `\\$', double it to `\\$\\$'.\\\\
    The next line contains the number of start activities, followed by, for each start activity, a line with the index of the start activity.\\\\
    The next line contains the number of end activities, followed by, for each end activity, a line with the index of the end activity.\\\\
    The next line contains the number of edges, followed by, for each edge, a line with first the index of the source activity, then the `>` symbol, then the index of the target activity.
    
    For instance:
    \\lstinputlisting[language=ebilines, style=boxed]{../testfiles/a-b_star.dfm}", dfg_format_comparison!());

    const IMPORTER_PARAMETERS: &[ImporterParameter] = &[];

    fn import_as_object(
        reader: &mut dyn std::io::prelude::BufRead,
        parameter_values: &ImporterParameterValues,
    ) -> Result<EbiObject> {
        Ok(EbiObject::DirectlyFollowsModel(Self::import(
            reader,
            parameter_values,
        )?))
    }

    fn import(
        reader: &mut dyn std::io::prelude::BufRead,
        _: &ImporterParameterValues,
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
                "first line should be exactly `{}`, but found `{}`",
                HEADER,
                lreader.get_last_line()
            ));
        }

        //read empty traces
        let empty_traces = lreader
            .next_line_bool()
            .context("could not read whether the model supports empty traces")?;

        //read activities
        let number_of_nodes = lreader
            .next_line_index()
            .context("could not read the number of nodes")?;
        let mut activity_key = ActivityKey::new();
        let mut node_2_activity = vec![];
        for activity in 0..number_of_nodes {
            let activity = lreader
                .next_activity(&mut activity_key)
                .with_context(|| format!("could not read activity {}", activity))?;
            node_2_activity.push(activity);
        }

        //read start activities
        let mut start_nodes = vec![false; number_of_nodes];
        let number_of_start_nodes = lreader
            .next_line_index()
            .context("could not read the number of start nodes")?;
        for i in 0..number_of_start_nodes {
            let start_node = lreader
                .next_line_index()
                .with_context(|| format!("could not read start node {}", i))?;
            start_nodes[start_node] = true;
        }

        //read end activities
        let mut end_nodes = vec![false; number_of_nodes];
        let number_of_end_nodes = lreader
            .next_line_index()
            .context("could not read the number of end nodes")?;
        for i in 0..number_of_end_nodes {
            let end_node = lreader
                .next_line_index()
                .with_context(|| format!("could not read end node {}", i))?;
            end_nodes[end_node] = true;
        }

        let mut result = Self {
            activity_key,
            node_2_activity,
            empty_traces,
            sources: vec![],
            targets: vec![],
            start_nodes,
            end_nodes,
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
            if let Some((source, target)) = arr.next_tuple() {
                let source = AutomatonState::of(
                    source
                        .parse::<usize>()
                        .with_context(|| format!("could not read source of edge {}", e))?,
                );
                let target = AutomatonState::of(
                    target
                        .parse::<usize>()
                        .with_context(|| format!("could not read target of edge {}", e))?,
                );
                result.add_edge(source, target);
            } else {
                return Err(anyhow!(
                    "could not read edge, which must be two numbers separated by >"
                ));
            }
        }

        Ok(result)
    }
}
from_string!(DirectlyFollowsModel);

impl Display for DirectlyFollowsModel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}", HEADER)?;

        writeln!(f, "# empty trace\n{}", self.empty_traces)?;

        //activities
        writeln!(f, "# number of activites\n{}", self.node_2_activity.len())?;
        for (a, activity) in self.node_2_activity.iter().enumerate() {
            writeln!(f, "#activity {}", a)?;
            LineReader::write_multiline_activity(f, activity, &self.activity_key)?;
        }

        //start activities
        writeln!(f, "# number of start activites\n{}", self.start_nodes.len())?;
        for (i, start_activity) in self.start_nodes.iter().enumerate() {
            writeln!(f, "# start activity {}\n{}", i, start_activity)?;
        }

        //end activities
        writeln!(f, "# number of end activites\n{}", self.end_nodes.len())?;
        for (i, end_activity) in self.end_nodes.iter().enumerate() {
            writeln!(f, "# end activity {}\n{}", i, end_activity)?;
        }

        //edges
        writeln!(f, "# number of edges\n{}\n# edges", self.sources.len())?;
        for (source, target) in self.sources.iter().zip(self.targets.iter()) {
            writeln!(f, "{}>{}", source, target)?;
        }

        Ok(write!(f, "")?)
    }
}

impl Graphable for DirectlyFollowsModel {
    fn to_dot(&self) -> Result<layout::topo::layout::VisualGraph> {
        let mut graph = VisualGraph::new(layout::core::base::Orientation::LeftToRight);

        //source + sink
        let source = graphable::create_place(&mut graph, "");
        let sink = graphable::create_place(&mut graph, "");

        //empty traces
        if self.empty_traces {
            graphable::create_edge(&mut graph, &source, &sink, "");
        }

        //nodes
        let mut nodes = vec![];
        for n in &self.node_2_activity {
            nodes.push(graphable::create_transition(
                &mut graph,
                self.activity_key.get_activity_label(n),
                "",
            ));
        }

        //start activities
        for (activity, is) in self.start_nodes.iter().enumerate() {
            if *is {
                graphable::create_edge(&mut graph, &source, &nodes[activity], "");
            }
        }

        //end activities
        for (activity, is) in self.end_nodes.iter().enumerate() {
            if *is {
                graphable::create_edge(&mut graph, &nodes[activity], &sink, "");
            }
        }

        //edges
        for (source, target) in self.sources.iter().zip(self.targets.iter()) {
            graphable::create_edge(&mut graph, &nodes[*source], &nodes[*target], "");
        }

        Ok(graph)
    }
}

impl Exportable for DirectlyFollowsModel {
    fn export_from_object(object: EbiObject, f: &mut dyn Write) -> Result<()> {
        match object {
            EbiObject::DirectlyFollowsModel(dfm) => Self::export(&dfm, f),
            EbiObject::DirectlyFollowsGraph(dfg) => {
                Self::export(&Into::<DirectlyFollowsModel>::into(dfg), f)
            }
            EbiObject::StochasticDirectlyFollowsModel(sdfm) => {
                Self::export(&Into::<DirectlyFollowsModel>::into(sdfm), f)
            }

            _ => Err(anyhow!(
                "cannot export {} {} as a directly follows model",
                object.get_type().get_article(),
                object.get_type()
            )),
        }
    }

    fn export(&self, f: &mut dyn std::io::Write) -> Result<()> {
        Ok(write!(f, "{}", self.to_string())?)
    }
}

impl Infoable for DirectlyFollowsModel {
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
            self.start_nodes.iter().filter(|x| **x).count()
        )?;
        writeln!(
            f,
            "Number of end nodes\t{}",
            self.end_nodes.iter().filter(|x| **x).count()
        )?;

        writeln!(f, "")?;
        self.activity_key().info(f)?;

        Ok(writeln!(f, "")?)
    }
}

#[cfg(any(test, feature = "testactivities"))]
impl TestActivityKey for DirectlyFollowsModel {
    fn test_activity_key(&self) {
        self.node_2_activity
            .iter()
            .for_each(|activity| self.activity_key().assert_activity_is_of_key(activity));
    }
}

impl AutomatonSemantics for DirectlyFollowsModel {
    fn initial_state(&self) -> Option<AutomatonState> {
        if self.node_2_activity.is_empty() && !self.empty_traces {
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
            .zip(self.targets.iter())
            .enumerate()
            .map(|(transition, (source, target))| {
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
            .end_nodes
            .iter()
            .enumerate()
            .filter(|(_, is)| **is)
            .map(move |(state, _)| (end_transition, AutomatonState::of(state), end_state, None));

        //start transitions
        let start_transition = self.sources.len() + 1;
        let start_state = AutomatonState::of(self.node_2_activity.len());
        let it3 = self
            .end_nodes
            .iter()
            .enumerate()
            .filter(|(_, is)| **is)
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
                .start_nodes
                .iter()
                .enumerate()
                .filter_map(|(node, is)| {
                    if *is {
                        Some(self.sources.len() + 1 + node)
                    } else {
                        None
                    }
                })
                .collect::<Vec<_>>();

            if self.empty_traces {
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
                result.push(i);
                i += 1;
            }

            //add transition to final state
            if self.end_nodes[state] {
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
