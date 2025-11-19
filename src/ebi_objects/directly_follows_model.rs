use anyhow::{Context, Result, anyhow};
use ebi_derive::ActivityKey;
use itertools::Itertools;
use layout::topo::layout::VisualGraph;
use std::{cmp::Ordering, fmt::Display, io::Write};

use crate::{
    Activity, ActivityKey, ActivityKeyTranslator, Exportable, Graphable, HasActivityKey,
    Importable, Infoable, TranslateActivityKey,
    constants::ebi_object::EbiObject,
    line_reader::LineReader,
    traits::{
        graphable,
        importable::{ImporterParameter, ImporterParameterValues, from_string},
    },
};

use super::stochastic_directly_follows_model::NodeIndex;

pub const HEADER: &str = "directly follows model";

#[macro_export]
macro_rules! format_comparison {
    () => {"
    
    The following table gives an overview of several directly follows-based file types and their features:
    \\begin{center}
    \\begin{tabularx}{\\linewidth}{Xlll}
        \\toprule
        File type & stochastic & multiple nodes with the same label & file syntax \\\\
        \\midrule
        \\hyperref[filehandler:directly follows graph]{directly follows graph (.dfg)} & yes & no & JSON \\\\
        \\hyperref[filehandler:directly follows model]{directly follows model (.dfm)} & no & yes & line-based \\\\
        \\hyperref[filehandler:stochastic directly follows model]{stochastic directly follows model (.sdfm)} & yes & yes & line-based \\\\
        \\bottomrule
    \\end{tabularx}
    \\end{center}"}
}

#[derive(ActivityKey, Debug, Clone)]
pub struct DirectlyFollowsModel {
    pub activity_key: ActivityKey,
    pub node_2_activity: Vec<Activity>,
    pub empty_traces: bool,
    pub sources: Vec<NodeIndex>, //edge -> source of edge
    pub targets: Vec<NodeIndex>, //edge -> target of edge
    pub start_nodes: Vec<bool>,  //node -> how often observed
    pub end_nodes: Vec<bool>,    //node -> how often observed
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

    pub fn has_empty_traces(&self) -> bool {
        self.empty_traces
    }

    pub fn can_terminate_in_node(&self, node: NodeIndex) -> bool {
        self.end_nodes[node]
    }

    pub fn number_of_start_nodes(&self) -> usize {
        self.start_nodes
            .iter()
            .fold(0, |a, b| if *b { a + 1 } else { a })
    }

    pub fn number_of_end_nodes(&self) -> usize {
        self.start_nodes
            .iter()
            .fold(0, |a, b| if *b { a + 1 } else { a })
    }

    pub fn is_start_node(&self, node: NodeIndex) -> bool {
        self.start_nodes[node]
    }

    pub fn is_end_node(&self, node: NodeIndex) -> bool {
        self.end_nodes[node]
    }

    pub fn can_execute_edge(&self, _edge: usize) -> bool {
        true
    }

    pub fn get_max_state(&self) -> usize {
        self.node_2_activity.len() + 2
    }

    pub fn add_empty_trace(&mut self) {
        self.empty_traces = true;
    }

    pub fn add_node(&mut self, activity: Activity) -> NodeIndex {
        let index = self.node_2_activity.len();
        self.node_2_activity.push(activity);
        self.start_nodes.push(false);
        self.end_nodes.push(false);
        index
    }

    pub fn add_edge(&mut self, source: NodeIndex, target: NodeIndex) {
        let (found, from) = self.binary_search(source, target);
        if found {
            //edge already present
        } else {
            //new edge
            self.sources.insert(from, source);
            self.targets.insert(from, target);
        }
    }

    pub fn binary_search(&self, source: NodeIndex, target: NodeIndex) -> (bool, usize) {
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
        source1: NodeIndex,
        target1: NodeIndex,
        source2: NodeIndex,
        target2: NodeIndex,
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
    This first line is exactly `directly follows model'.\\
    The second line is a boolean indicating whether the model supports empty traces.\\
    The third line is the number of activities in the model.\\
    The following lines each contain an activity. Duplicated labels are accepted.\\
    The next line contains the number of start activities, followed by, for each start activity, a line with the index of the start activity.\\
    The next line contains the number of end activities, followed by, for each end activity, a line with the index of the end activity.\\
    The next line contains the number of edges, followed by, for each edge, a line with first the index of the source activity, then the `>` symbol, then the index of the target activity.
    
    For instance:
    \\lstinputlisting[language=ebilines, style=boxed]{../testfiles/a-b_star.dfm}", format_comparison!());

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
    ) -> anyhow::Result<Self>
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
            let label = lreader
                .next_line_string()
                .with_context(|| format!("could not read activity {}", activity))?;
            let activity = activity_key.process_activity(&label);
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
                let source = source
                    .parse::<usize>()
                    .with_context(|| format!("could not read source of edge {}", e))?;
                let target = target
                    .parse::<usize>()
                    .with_context(|| format!("could not read target of edge {}", e))?;
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
            writeln!(
                f,
                "#activity {}\n{}",
                a,
                self.activity_key.get_activity_label(activity)
            )?;
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
        writeln!(f, "Number of start nodes\t{}", self.number_of_start_nodes())?;
        writeln!(f, "Number of end nodes\t{}", self.number_of_end_nodes())?;

        writeln!(f, "")?;
        self.activity_key().info(f)?;

        Ok(writeln!(f, "")?)
    }
}
