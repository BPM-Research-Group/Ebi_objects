use super::stochastic_process_tree::StochasticProcessTree;
use crate::{
    Activity, ActivityKey, ActivityKeyTranslator, EbiObject, Exportable, Graphable, HasActivityKey,
    Importable, Infoable, TranslateActivityKey,
    ebi_objects::labelled_petri_net::TransitionIndex,
    line_reader::LineReader,
    traits::{
        graphable,
        importable::{ImporterParameter, ImporterParameterValues, from_string},
    },
};
use anyhow::{Context, Error, Result, anyhow};
use ebi_derive::ActivityKey;
use layout::{adt::dag::NodeHandle, topo::layout::VisualGraph};
use std::{
    fmt::Display,
    ops::{Index, IndexMut},
    str::FromStr,
};
use strum::IntoEnumIterator;
use strum_macros::EnumIter;

pub const HEADER: &str = "process tree";

#[derive(Debug, ActivityKey, Clone)]
pub struct ProcessTree {
    pub activity_key: ActivityKey,
    pub tree: Vec<Node>,
    pub transition2node: Vec<usize>,
}

impl ProcessTree {
    pub fn number_of_leaves(&self) -> usize {
        self.tree.iter().filter(|node| node.is_leaf()).count()
    }

    fn node_to_string(
        &self,
        indent: usize,
        node: usize,
        f: &mut std::fmt::Formatter<'_>,
    ) -> Result<usize> {
        let id = "\t".repeat(indent);
        match &self.tree[node] {
            Node::Tau => {
                writeln!(f, "{}tau", id)?;
                Ok(node + 1)
            }
            Node::Activity(activity) => {
                writeln!(
                    f,
                    "{}activity {}",
                    id,
                    self.activity_key.get_activity_label(&activity)
                )?;
                Ok(node + 1)
            }
            Node::Operator(operator, number_of_children) => {
                writeln!(f, "{}{}", id, operator.to_string())?;
                writeln!(
                    f,
                    "{}# number of children\n{}{}",
                    id, id, number_of_children
                )?;
                let mut child = node + 1;
                for _ in 0..*number_of_children {
                    child = self.node_to_string(indent + 1, child, f)?;
                }
                Ok(child)
            }
        }
    }

    fn node_to_dot(
        &self,
        graph: &mut VisualGraph,
        node: usize,
        entry: &NodeHandle,
        exit: &NodeHandle,
    ) -> usize {
        match self.tree[node] {
            Node::Tau => {
                graphable::create_edge(graph, entry, exit, "");
                node + 1
            }
            Node::Activity(activity) => {
                let transition = graphable::create_transition(
                    graph,
                    self.activity_key.get_activity_label(&activity),
                    "",
                );
                graphable::create_edge(graph, entry, &transition, "");
                graphable::create_edge(graph, &transition, exit, "");
                node + 1
            }
            Node::Operator(Operator::Xor, number_of_children) => {
                let mut child = node + 1;
                for _ in 0..number_of_children {
                    child = ProcessTree::node_to_dot(&self, graph, child, entry, exit);
                }
                child
            }
            Node::Operator(Operator::Sequence, number_of_children) => {
                let intermediate_nodes = (0..(number_of_children - 1))
                    .map(|_| graphable::create_dot(graph))
                    .collect::<Vec<_>>();

                let mut child = node + 1;
                for i in 0..number_of_children {
                    let child_entry = if i == 0 {
                        entry
                    } else {
                        &intermediate_nodes[i - 1]
                    };
                    let child_exit = if i == number_of_children - 1 {
                        exit
                    } else {
                        &intermediate_nodes[i]
                    };

                    child = ProcessTree::node_to_dot(&self, graph, child, child_entry, child_exit);
                }
                child
            }
            Node::Operator(Operator::Concurrent, number_of_children) => {
                let split = graphable::create_gateway(graph, "+");
                graphable::create_edge(graph, entry, &split, "");
                let join = graphable::create_gateway(graph, "+");
                graphable::create_edge(graph, &join, exit, "");

                let mut child = node + 1;
                for _ in 0..number_of_children {
                    child = ProcessTree::node_to_dot(&self, graph, child, &split, &join);
                }
                child
            }
            Node::Operator(Operator::Or, number_of_children) => {
                let split = graphable::create_gateway(graph, "o");
                graphable::create_edge(graph, entry, &split, "");
                let join = graphable::create_gateway(graph, "o");
                graphable::create_edge(graph, &join, exit, "");

                let mut child = node + 1;
                for _ in 0..number_of_children {
                    child = ProcessTree::node_to_dot(&self, graph, child, &split, &join);
                }
                child
            }
            Node::Operator(Operator::Interleaved, number_of_children) => {
                let split = graphable::create_gateway(graph, "↔");
                graphable::create_edge(graph, entry, &split, "");
                let join = graphable::create_gateway(graph, "↔");
                graphable::create_edge(graph, &join, exit, "");

                let mut child = node + 1;
                for _ in 0..number_of_children {
                    child = ProcessTree::node_to_dot(&self, graph, child, &split, &join);
                }
                child
            }
            Node::Operator(Operator::Loop, number_of_children) => {
                let split = graphable::create_dot(graph);
                graphable::create_edge(graph, entry, &split, "");
                let join = graphable::create_dot(graph);
                graphable::create_edge(graph, &join, exit, "");

                let mut child = node + 1;

                child = ProcessTree::node_to_dot(&self, graph, child, &split, &join);

                if number_of_children == 1 {
                    graphable::create_edge(graph, &join, &split, "");
                } else {
                    for _ in 1..number_of_children {
                        child = ProcessTree::node_to_dot(&self, graph, child, &join, &split);
                    }
                }
                child
            }
        }
    }

    ///read one node, recursively
    fn string_to_tree(
        lreader: &mut LineReader<'_>,
        tree: &mut Vec<Node>,
        activity_key: &mut ActivityKey,
        root: bool,
    ) -> Result<()> {
        let node_type_line = match lreader.next_line_string().with_context(|| {
            format!(
                "Failed to read node {} at line {}",
                tree.len(),
                lreader.get_last_line_number()
            )
        }) {
            Ok(x) => x,
            Err(e) => {
                if root {
                    //The root may be missing: then, we have an empty tree.
                    return Ok(());
                } else {
                    return Err(e);
                }
            }
        };

        if node_type_line.trim_start().starts_with("tau") {
            tree.push(Node::Tau);
        } else if node_type_line.trim_start().starts_with("activity ") {
            let label = node_type_line.trim_start()[9..].to_string();
            let activity = activity_key.process_activity(&label);
            tree.push(Node::Activity(activity));
        } else if let Ok(operator) = node_type_line.trim_start().trim_end().parse::<Operator>() {
            let number_of_children = lreader.next_line_index().with_context(|| {
                format!(
                    "failed to read number of children for node {} at line {}",
                    tree.len(),
                    lreader.get_last_line_number()
                )
            })?;
            if number_of_children < 1 {
                return Err(anyhow!(
                    "loop node ending at node {} at line {} has no children",
                    tree.len(),
                    lreader.get_last_line_number()
                ));
            }
            tree.push(Node::Operator(operator, number_of_children));
            for _ in 0..number_of_children {
                Self::string_to_tree(lreader, tree, activity_key, false)?;
            }
        } else if root && node_type_line.trim_start().is_empty() {
            //empty tree
            return Ok(());
        } else {
            return Err(anyhow!(
                "could not parse type of node {} at line {}. Expected `tau`, `activity`, `concurrent`, `interleaved`, `or`, `sequence` or `xor`",
                tree.len(),
                lreader.get_last_line_number()
            ));
        }

        Ok(())
    }
}

macro_rules! tree {
    ($t:ident, $u:ident, $v:ident) => {
        impl $t {
            pub fn get_number_of_nodes(&self) -> usize {
                return self.tree.len();
            }

            pub fn get_node(&self, node: usize) -> Option<&Node> {
                self.tree.get(node)
            }

            pub fn root(&self) -> usize {
                0
            }

            pub fn get_node_of_transition(&self, transition: TransitionIndex) -> Result<&Node> {
                self.tree
                    .get(
                        *self
                            .transition2node
                            .get(transition)
                            .ok_or_else(|| anyhow!("Transition does not exist."))?,
                    )
                    .ok_or_else(|| anyhow!("Node does not exist."))
            }

            /**
             * Returns the parent of node. Notice that
             * this is an expensive operation; avoid if possible.
             *
             * @param node
             * @return The parent of node, and the rank of the child
             */
            pub fn get_parent(&self, node: usize) -> Option<(usize, usize)> {
                if node == 0 {
                    return None;
                }

                let mut potential_parent = node - 1;
                while self.traverse(potential_parent) <= node {
                    potential_parent -= 1;
                }

                let child_rank = self.get_child_rank_with(potential_parent, node)?;

                Some((potential_parent, child_rank))
            }

            /**
             *
             * @param parent
             * @param grandChild
             * @return The number of the child within parent that contains grandChild.
             *         If grandChild is not a child of parent, will return -1.
             */
            pub fn get_child_rank_with(&self, parent: usize, grand_child: usize) -> Option<usize> {
                let mut child_rank = 0;
                for child in self.get_children(parent) {
                    if self.is_parent_of(child, grand_child) {
                        return Some(child_rank);
                    }
                    child_rank += 1;
                }
                None
            }

            pub fn get_children(&self, node: usize) -> $u<'_> {
                $u::new(self, node)
            }

            pub fn get_parents(&self, node: usize) -> $v<'_> {
                $v::new(self, node)
            }

            pub fn get_descendants(&self, node: usize) -> &[Node] {
                let next = self.traverse(node);
                &self.tree[node..next]
            }

            /**
             *
             * @param parent
             * @param child
             * @return Whether the child is a direct or indirect child of parent.
             */
            pub fn is_parent_of(&self, parent: usize, child: usize) -> bool {
                if parent > child {
                    return false;
                }
                return self.traverse(parent) > child;
            }

            /**
             * Find the next node of this node: the next sibling, and if that is not available, the next sibling up the tree.
             * May return a non-existing node if there is no sibling.
             */
            pub fn traverse(&self, node: usize) -> usize {
                match self.tree[node] {
                    Node::Tau => node + 1,
                    Node::Activity(_) => node + 1,
                    Node::Operator(_, number_of_children) => {
                        let mut n = node + 1;
                        for _ in 0..number_of_children {
                            n = self.traverse(n);
                        }
                        n
                    }
                }
            }

            pub fn get_child(&self, parent: usize, child_rank: usize) -> usize {
                let mut i = parent + 1;
                for _ in 0..child_rank {
                    i = self.traverse(i);
                }
                return i;
            }

            pub fn get_number_of_children(&self, parent: usize) -> Option<usize> {
                match self.tree.get(parent)? {
                    Node::Tau => Some(0),
                    Node::Activity(_) => Some(0),
                    Node::Operator(_, number_of_children) => Some(*number_of_children),
                }
            }

            pub fn node_to_transition(&self, node: usize) -> Option<usize> {
                let mut transitions = 0;
                let mut last = false;
                for node in self.tree.iter().take(node + 1) {
                    match node {
                        Node::Activity(_) | Node::Tau => {
                            transitions += 1;
                            last = true
                        }
                        _ => last = false,
                    }
                }

                if last { Some(transitions - 1) } else { None }
            }
        }

        impl TranslateActivityKey for $t {
            fn translate_using_activity_key(&mut self, to_activity_key: &mut ActivityKey) {
                let translator = ActivityKeyTranslator::new(&self.activity_key, to_activity_key);
                self.tree.iter_mut().for_each(|node| {
                    if let Node::Activity(a) = node {
                        *a = translator.translate_activity(&a)
                    }
                });
                self.activity_key = to_activity_key.clone();
            }
        }

        impl Infoable for $t {
            fn info(&self, f: &mut impl std::io::Write) -> Result<()> {
                writeln!(f, "Number of nodes\t\t{}", self.get_number_of_nodes())?;
                writeln!(
                    f,
                    "Number of activities\t\t{}",
                    $t::activity_key(self).get_number_of_activities()
                )?;

                writeln!(f, "")?;
                self.activity_key().info(f)?;

                Ok(writeln!(f, "")?)
            }
        }

        impl Graphable for $t {
            fn to_dot(&self) -> Result<layout::topo::layout::VisualGraph> {
                let mut graph = VisualGraph::new(layout::core::base::Orientation::LeftToRight);
                let source = graphable::create_place(&mut graph, "");
                let sink = graphable::create_place(&mut graph, "");
                if !self.tree.is_empty() {
                    $t::node_to_dot(&self, &mut graph, 0, &source, &sink);
                }
                Ok(graph)
            }
        }

        pub struct $u<'a> {
            //children iterator
            tree: &'a $t,
            node: usize,
            now: Option<usize>,
            next: usize,
            count: usize,
        }

        impl<'a> $u<'a> {
            fn new(tree: &'a $t, node: usize) -> Self {
                Self {
                    tree: tree,
                    node: node,
                    now: None,
                    next: node + 1,
                    count: 0,
                }
            }
        }

        impl<'a> Iterator for $u<'a> {
            type Item = usize;

            fn next(&mut self) -> Option<Self::Item> {
                if self.count >= self.tree.get_number_of_children(self.node)? {
                    return None;
                }
                self.count += 1;
                self.now = Some(self.next);
                self.next = self.tree.traverse(self.now.unwrap());
                Some(self.now.unwrap())
            }
        }

        pub struct $v<'a> {
            //parents iterator
            tree: &'a $t,
            node: Option<(usize, usize)>,
        }

        impl<'a> $v<'a> {
            fn new(tree: &'a $t, node: usize) -> Self {
                Self {
                    tree: tree,
                    node: tree.get_parent(node),
                }
            }
        }

        impl<'a> Iterator for $v<'a> {
            type Item = (usize, usize);

            fn next(&mut self) -> Option<Self::Item> {
                if let Some((node, child_rank)) = self.node {
                    self.node = self.tree.get_parent(node);

                    Some((node, child_rank))
                } else {
                    None
                }
            }
        }
    };
}

tree!(ProcessTree, ChildrenIterator, ParentsIterator);
tree!(
    StochasticProcessTree,
    StochasticChildrenIterator,
    StochasticParentsIterator
);

impl Display for ProcessTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}", HEADER)?;
        if !self.tree.is_empty() {
            let _ = self.node_to_string(0, 0, f);
        };
        write!(f, "")
    }
}

impl Importable for ProcessTree {
    const FILE_FORMAT_SPECIFICATION_LATEX: &str = "A process tree is a line-based structure. Lines starting with a \\# are ignored.
    This first line is exactly `process tree'.
    The subsequent lines contain the nodes:
    Each node is either:
    \\begin{itemize}
        \\item A line with the word `activity' followed on the same line by a space and the label of the activity leaf;
        \\item The word `tau';
        \\item The name of an operator (`sequence', `xor', `concurrent', `loop', `interleaved', or `or') on its own line.
        The line thereafter contains the number of children of the node, after which the nodes are given.
        An operator node must have at least one child.
    \\end{itemize}
    Indentation of nodes is allowed, but not mandatory.
    
    For instance:
    \\lstinputlisting[language=ebilines, style=boxed]{../testfiles/all_operators.ptree}";

    const IMPORTER_PARAMETERS: &[ImporterParameter] = &[];

    fn import_as_object(
        reader: &mut dyn std::io::BufRead,
        parameter_values: &ImporterParameterValues,
    ) -> Result<EbiObject> {
        Ok(EbiObject::ProcessTree(Self::import(
            reader,
            parameter_values,
        )?))
    }

    fn import(reader: &mut dyn std::io::BufRead, _: &ImporterParameterValues) -> Result<Self>
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
        let mut tree = vec![];
        Self::string_to_tree(&mut lreader, &mut tree, &mut activity_key, true)?;

        Ok((activity_key, tree).into())
    }
}
from_string!(ProcessTree);

impl Exportable for ProcessTree {
    fn export_from_object(object: EbiObject, f: &mut dyn std::io::Write) -> Result<()> {
        match object {
            EbiObject::ProcessTree(lpn) => lpn.export(f),
            EbiObject::StochasticProcessTree(lpn) => {
                <StochasticProcessTree as Into<ProcessTree>>::into(lpn).export(f)
            }
            _ => Err(anyhow!(
                "cannot export {} {} as a process tree",
                object.get_type().get_article(),
                object.get_type()
            )),
        }
    }

    fn export(&self, f: &mut dyn std::io::Write) -> Result<()> {
        Ok(write!(f, "{}", self)?)
    }
}

#[derive(Debug, Clone)]
pub enum Node {
    Tau,
    Activity(Activity),
    Operator(Operator, usize), //type, number of children
}

impl Node {
    pub fn is_leaf(&self) -> bool {
        match self {
            Self::Tau | Self::Activity(_) => true,
            Self::Operator(_, _) => false,
        }
    }

    pub fn set_number_of_children(&mut self, number_of_children: usize) -> Result<()> {
        if let Self::Operator(_, old_number_of_children) = self {
            *old_number_of_children = number_of_children;
            Ok(())
        } else {
            Err(anyhow!(
                "attempted to alter the number of children of an activity or a tau"
            ))
        }
    }
}

#[derive(EnumIter, Debug, Clone, Copy)]
pub enum Operator {
    Xor,
    Sequence,
    Interleaved,
    Concurrent,
    Or,
    Loop,
}

impl Operator {
    pub fn to_string(&self) -> &str {
        match self {
            Operator::Xor => "xor",
            Operator::Sequence => "sequence",
            Operator::Interleaved => "interleaved",
            Operator::Concurrent => "concurrent",
            Operator::Or => "or",
            Operator::Loop => "loop",
        }
    }
}

impl FromStr for Operator {
    type Err = Error;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        for op in Operator::iter() {
            if s == op.to_string() {
                return Ok(op);
            }
        }
        return Err(anyhow!("operator not recognised"));
    }
}

// ======= semantics functions =======
// Semantics belongs in the Ebi crate, not here in Ebi_objects,
// however we use them to create a state space in several conversions.
// As such, its bases functions are here.

#[derive(Clone, strum_macros::Display, Debug, Eq, PartialEq, Hash, PartialOrd, Ord)]
pub enum NodeState {
    Enabled,
    Started,
    Closed,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash, PartialOrd, Ord)]
pub struct TreeMarking {
    pub(crate) terminated: bool,
    pub(crate) states: Vec<NodeState>,
}

impl TreeMarking {
    pub fn get(&self, index: usize) -> Option<&NodeState> {
        self.states.get(index)
    }
}

impl Display for TreeMarking {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}, terminated: {}", self.states, self.terminated)
    }
}

impl Index<usize> for TreeMarking {
    type Output = NodeState;

    fn index(&self, index: usize) -> &Self::Output {
        self.states.index(index)
    }
}

impl IndexMut<usize> for TreeMarking {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        self.states.index_mut(index)
    }
}

#[macro_export]
macro_rules! tree_semantics {
    ($t:ident) => {
        pub fn get_initial_state(tree: &$t) -> Option<TreeMarking> {
            if tree.tree.is_empty() {
                None
            } else {
                let mut state = TreeMarking {
                    states: vec![NodeState::Closed; tree.get_number_of_nodes()],
                    terminated: false,
                };
                enable_node(tree, &mut state, tree.root());
                Some(state)
            }
        }

        /// Returns whether it is possible that this node now terminates (true), or that a leaf has to be executed first (false).
        pub(crate) fn can_terminate(tree: &$t, state: &TreeMarking, node: usize) -> bool {
            match tree.tree[node] {
                Node::Tau => state[node] == NodeState::Closed,
                Node::Activity(_) => state[node] == NodeState::Closed,
                Node::Operator(Operator::Concurrent, _)
                | Node::Operator(Operator::Interleaved, _) => {
                    //these nodes can terminate if all of their children are either closed or can terminate
                    tree.get_children(node).all(|child| {
                        state[child] == NodeState::Closed || can_terminate(tree, state, child)
                    })
                }
                Node::Operator(Operator::Or, _) => {
                    //an or can terminate if at least one child can terminate, and the others can be withdrawn
                    let mut one_child_closed = false;
                    for child in tree.get_children(node) {
                        if can_terminate(tree, state, child) {
                            one_child_closed = true;
                        } else if !can_withdraw_enablement(tree, state, child) {
                            //if there is one child that is not closed and not withdrawn, we cannot terminate the or
                            return false;
                        }
                    }

                    one_child_closed
                }
                Node::Operator(Operator::Loop, number_of_children) => {
                    let body_child = tree.get_child(node, 0);
                    if state[node] == NodeState::Closed {
                        //if the loop is closed, it can terminate
                        return true;
                    }
                    if state[body_child] == NodeState::Enabled {
                        //the first child is enabled, which means that the loop cannot terminate in this state
                        return false;
                    }

                    for child_rank in 1..number_of_children {
                        let redo_child = tree.get_child(node, child_rank);
                        //all the redo children must be able to withdraw enablement
                        if !can_withdraw_enablement(tree, state, redo_child) {
                            return false;
                        }
                    }

                    return true;
                }
                Node::Operator(Operator::Sequence, number_of_children) => {
                    //a sequence node can terminate if all its non-last children are closed and the last child can terminate
                    for child_rank in 0..number_of_children - 1 {
                        if state[tree.get_child(node, child_rank)] != NodeState::Closed {
                            return false;
                        }
                    }
                    can_terminate(tree, state, tree.get_child(node, number_of_children - 1))
                }
                Node::Operator(Operator::Xor, _) => {
                    //an xor can terminate if all of its children are closed or can terminate
                    tree.get_children(node).all(|child| {
                        state[child] == NodeState::Closed || can_terminate(tree, state, child)
                    })
                }
            }
        }

        /**
         * Returns whether it is possible to withdraw the enablement.
         */
        fn can_withdraw_enablement(_tree: &$t, state: &TreeMarking, node: usize) -> bool {
            state[node] == NodeState::Enabled
        }

        pub(crate) fn can_execute(tree: &$t, state: &TreeMarking, node: usize) -> bool {
            match tree.tree.get(node) {
                Some(Node::Activity(_)) => {}
                Some(Node::Tau) => {}
                _ => return false,
            }

            if let Some(NodeState::Closed) = state.get(node) {
                return false;
            }
            if let Some(NodeState::Started) = state.get(node) {
                return false;
            }

            //for every interleaved parent, check whether we're not executing two nodes concurrently
            let mut previous_parent = node;
            for (parent, _) in tree.get_parents(node) {
                if let Some(Node::Operator(Operator::Interleaved, _)) = tree.tree.get(parent) {
                    //count the number of started children
                    let started_children = tree.get_children(parent).fold(0, |count, child| {
                        if state[child] == NodeState::Started && !can_terminate(tree, state, child)
                        {
                            count + 1
                        } else {
                            count
                        }
                    });

                    if started_children == 0 {
                        //this is the first starting child; no problem
                    } else if started_children == 1 {
                        //there is already a child of this interleaved parent started
                        if state[previous_parent] != NodeState::Started {
                            //another child already started; this node cannot fire now
                            return false;
                        }
                    } else {
                        unreachable!()
                    }
                }

                previous_parent = parent;
            }

            true
        }

        fn enable_node(tree: &$t, state: &mut TreeMarking, node: usize) {
            state[node] = NodeState::Enabled;

            match tree.tree[node] {
                Node::Tau => {}
                Node::Activity(_) => {}
                Node::Operator(Operator::Concurrent, _)
                | Node::Operator(Operator::Interleaved, _)
                | Node::Operator(Operator::Or, _)
                | Node::Operator(Operator::Xor, _) => {
                    //enable all children
                    for child in tree.get_children(node) {
                        enable_node(tree, state, child);
                    }
                }
                Node::Operator(Operator::Sequence, _) | Node::Operator(Operator::Loop, _) => {
                    //enable the first child
                    enable_node(tree, state, tree.get_child(node, 0));
                }
            }
        }

        /// enable the nodes that are sequentially next from this node
        fn enable_next_sequential_nodes(tree: &$t, state: &mut TreeMarking, node: usize) {
            if let Some((parent, child_rank)) = tree.get_parent(node) {
                match tree.tree[parent] {
                    Node::Tau => unreachable!(),
                    Node::Activity(_) => unreachable!(),
                    Node::Operator(Operator::Xor, _) => {
                        //propagate to parent
                        enable_next_sequential_nodes(tree, state, parent);
                    }
                    Node::Operator(Operator::Concurrent, _)
                    | Node::Operator(Operator::Or, _)
                    | Node::Operator(Operator::Interleaved, _) => {
                        if can_terminate(tree, state, parent) {
                            //this parent can terminate, so we propagate to parent
                            enable_next_sequential_nodes(tree, state, parent);
                        } else {
                            //this parent cannot yet terminate, so there is nothing to enable
                        }
                    }
                    Node::Operator(Operator::Loop, number_of_children) => {
                        if child_rank == 0 {
                            //enable the redo children and the next sequential node
                            for child_rank in 1..number_of_children {
                                enable_node(tree, state, tree.get_child(parent, child_rank));
                            }
                            enable_next_sequential_nodes(tree, state, parent);
                        } else {
                            //we were a redo child, enable the body child
                            enable_node(tree, state, tree.get_child(parent, 0));
                        }
                    }
                    Node::Operator(Operator::Sequence, number_of_children) => {
                        if child_rank == number_of_children - 1 {
                            //this was the last child, the sequence would end here; propagate to parent
                            enable_next_sequential_nodes(tree, state, parent);
                        } else {
                            //enable the next child
                            enable_node(tree, state, tree.get_child(parent, child_rank + 1));
                        }
                    }
                }
            }
        }

        /// withdraws the enablement of the next sequential nodes
        fn withdraw_enablement_next_sequential_nodes(
            tree: &$t,
            state: &mut TreeMarking,
            node: usize,
        ) {
            if let Some((parent, child_rank)) = tree.get_parent(node) {
                match tree.tree[parent] {
                    Node::Tau => unreachable!(),
                    Node::Activity(_) => unreachable!(),
                    Node::Operator(Operator::Sequence, number_of_children) => {
                        if child_rank < number_of_children - 1 {
                            //we were a non-last child; withdraw the enablement of our next sibling
                            withdraw_enablement(
                                tree,
                                state,
                                tree.get_child(parent, child_rank + 1),
                            );
                        } else {
                            //we are the last child; recurse upwards
                            withdraw_enablement_next_sequential_nodes(tree, state, parent);
                        }
                    }
                    Node::Operator(Operator::Concurrent, _)
                    | Node::Operator(Operator::Or, _)
                    | Node::Operator(Operator::Interleaved, _)
                    | Node::Operator(Operator::Xor, _) => {
                        //propagate to parent
                        withdraw_enablement_next_sequential_nodes(tree, state, parent);
                    }
                    Node::Operator(Operator::Loop, number_of_children) => {
                        if child_rank == 0 {
                            //we were the body child, withdraw redo children
                            for child_rank in 1..number_of_children {
                                withdraw_enablement(
                                    tree,
                                    state,
                                    tree.get_child(parent, child_rank),
                                );
                            }
                        } else {
                            //we were a redo child, withdraw the body child
                            withdraw_enablement(tree, state, tree.get_child(parent, 0));

                            //propagate to parent
                            withdraw_enablement_next_sequential_nodes(tree, state, parent);
                        }
                    }
                }
            }
        }

        fn close_node(tree: &$t, state: &mut TreeMarking, node: usize) {
            //close this node and all of its children
            for grandchild in node..tree.traverse(node) {
                state[grandchild] = NodeState::Closed;
            }

            //this may open another node, based on the operator of the parent
            if let Some((parent, child_rank)) = tree.get_parent(node) {
                match tree.tree[parent] {
                    Node::Tau => unreachable!(),
                    Node::Activity(_) => unreachable!(),
                    Node::Operator(Operator::Sequence, number_of_children) => {
                        //for a sequence parent, we enable the next child
                        // log::debug!("close node {}, parent is sequence node {}", node, parent);
                        if child_rank < number_of_children - 1 {
                            let next_child = tree.get_child(parent, child_rank + 1);
                            enable_node(tree, state, next_child);
                        } else {
                            //if there is no next child, we recurse on the parent
                            close_node(tree, state, parent);
                        }
                    }
                    Node::Operator(Operator::Concurrent, _)
                    | Node::Operator(Operator::Interleaved, _) => {
                        //for a concurrent or interleaved parent, the parent can be closed if all of its children have been closed
                        if tree
                            .get_children(parent)
                            .all(|child| state[child] == NodeState::Closed)
                        {
                            //close the parent
                            close_node(tree, state, parent);
                        }
                    }
                    Node::Operator(Operator::Or, _) => {
                        //for an or parent, the parent can be closed if all of its children have been closed
                        if tree
                            .get_children(parent)
                            .all(|child| state[child] == NodeState::Closed)
                        {
                            //close the parent
                            close_node(tree, state, parent);
                        }

                        //furthermore, we may continue with the next sequential sibling of the parent
                        enable_next_sequential_nodes(tree, state, parent);
                    }
                    Node::Operator(Operator::Xor, _) => {
                        //for a xor parent, the parent can be closed as we executed one of its children
                        close_node(tree, state, parent);
                    }
                    Node::Operator(Operator::Loop, number_of_children) => {
                        //for a loop parent, we open the next child(ren)
                        if child_rank == 0 {
                            //enable the siblings
                            for child_rank in 1..number_of_children {
                                enable_node(tree, state, tree.get_child(parent, child_rank));
                            }
                            //enable the exit
                            enable_next_sequential_nodes(tree, state, parent);
                        } else {
                            //enable the first child
                            enable_node(tree, state, tree.get_child(parent, 0));
                        }
                    }
                }
            }
        }

        fn withdraw_enablement(tree: &$t, state: &mut TreeMarking, node: usize) {
            for grandchild in node..tree.traverse(node) {
                state[grandchild] = NodeState::Closed;
            }
        }

        /// Starts executing a node.
        /// Recurses upwards to adjust enablement.
        fn start_node(tree: &$t, state: &mut TreeMarking, node: usize, child: Option<usize>) {
            match tree.tree[node] {
                Node::Tau
                | Node::Activity(_)
                | Node::Operator(Operator::Concurrent, _)
                | Node::Operator(Operator::Or, _)
                | Node::Operator(Operator::Sequence, _) => {
                    if state[node] != NodeState::Started {
                        state[node] = NodeState::Started;

                        //recurse to parent
                        if let Some((parent, _)) = tree.get_parent(node) {
                            start_node(tree, state, parent, Some(node));
                        }
                    }
                }
                Node::Operator(Operator::Interleaved, _) => {
                    if state[node] != NodeState::Started {
                        state[node] = NodeState::Started;

                        //recurse to parent
                        if let Some((parent, _)) = tree.get_parent(node) {
                            start_node(tree, state, parent, Some(node));
                        }
                    } else if let Some(from_child) = child {
                        //we are starting a non-first child of an interleaved node: withdraw enablement in all grand children
                        for child in tree.get_children(node) {
                            if child != from_child {
                                withdraw_enablement(tree, state, child);
                            }
                        }
                    } else {
                        unreachable!()
                    }
                }
                Node::Operator(Operator::Loop, number_of_children) => {
                    if state[node] != NodeState::Started {
                        state[node] = NodeState::Started;

                        //recurse to parent
                        if let Some((parent, _)) = tree.get_parent(node) {
                            start_node(tree, state, parent, Some(node));
                        }
                    } else {
                        //for a loop, even though it started before, we may need to withdraw enablement of next sequential nodes

                        if let Some(child) = child
                            && child > node + 1
                        {
                            //we are in a redo child: withdraw the next sequential node, which cannot fire now
                            withdraw_enablement_next_sequential_nodes(tree, state, node);
                            withdraw_enablement(tree, state, tree.get_child(node, 0));
                        } else {
                            //we are in a body child; withdraw the enablement of the redo children
                            for child_rank in 1..number_of_children {
                                withdraw_enablement(tree, state, tree.get_child(node, child_rank));
                            }
                        }

                        //recurse to parent
                        if let Some((parent, _)) = tree.get_parent(node) {
                            start_node(tree, state, parent, Some(node));
                        }
                    }
                }

                Node::Operator(Operator::Xor, _) => {
                    if state[node] != NodeState::Started {
                        state[node] = NodeState::Started;

                        //for an xor, the siblings of the child must be withdrawn
                        for child2 in tree.get_children(node) {
                            if let Some(child) = child {
                                if child2 != child {
                                    withdraw_enablement(tree, state, child2);
                                }
                            }
                        }
                        //recurse to parent
                        if let Some((parent, _)) = tree.get_parent(node) {
                            start_node(tree, state, parent, Some(node));
                        }
                    }
                }
            }
        }

        pub fn get_number_of_transitions(tree: &$t) -> usize {
            tree.tree.iter().filter(|node| node.is_leaf()).count() + 1 //the last transition is explicit termination, which is required by the semantics of Ebi
        }

        pub fn get_enabled_transitions(tree: &$t, state: &TreeMarking) -> Vec<TransitionIndex> {
            let mut result = vec![];

            for (transition_index, node) in tree.transition2node.iter().enumerate() {
                if can_execute(tree, state, *node) {
                    result.push(transition_index);
                }
            }

            if !state.terminated && can_terminate(tree, state, tree.root()) {
                result.push(tree.transition2node.len());
            }

            result
        }

        pub fn is_final_state(_tree: &$t, state: &TreeMarking) -> bool {
            state.terminated
        }

        pub fn get_transition_activity(tree: &$t, transition: TransitionIndex) -> Option<Activity> {
            let node = tree.transition2node.get(transition)?;
            match tree.tree[*node] {
                Node::Tau => None,
                Node::Activity(activity) => Some(activity),
                Node::Operator(_, _) => None,
            }
        }

        pub fn execute_transition(
            tree: &$t,
            state: &mut TreeMarking,
            transition: TransitionIndex,
        ) -> Result<()> {
            if transition >= tree.transition2node.len() {
                state.terminated = true;
                state.states.fill(NodeState::Closed);
            } else {
                let node = tree
                    .transition2node
                    .get(transition)
                    .ok_or_else(|| anyhow!("transition does not exist"))?;
                start_node(tree, state, *node, None);
                close_node(tree, state, *node);
            }
            // log::debug!("state after execution {}", state);
            Ok(())
        }
    };
}

tree_semantics!(ProcessTree);

#[cfg(test)]
mod tests {
    use crate::{
        HasActivityKey, ProcessTree, StochasticProcessTree,
        ebi_objects::process_tree::{
            execute_transition, get_enabled_transitions, get_initial_state, get_transition_activity,
        },
    };
    use std::fs;

    #[test]
    fn ptree_semantics_loop() {
        let fin1 =
            fs::read_to_string("testfiles/seq(a,xor(seq(f,and(c,b)),seq(f,loop(d,e))).sptree")
                .unwrap();
        let tree: ProcessTree = fin1.parse::<StochasticProcessTree>().unwrap().into();

        let ta = 0;
        let tf1 = 1;
        let tf2 = 4;
        let td = 5;
        let te = 6;
        let ttau = 7;
        let tfin = 8;

        assert_eq!(
            tree.activity_key()
                .deprocess_activity(&get_transition_activity(&tree, td).unwrap()),
            "d"
        );
        assert_eq!(
            tree.activity_key()
                .deprocess_activity(&get_transition_activity(&tree, te).unwrap()),
            "e"
        );
        assert!(get_transition_activity(&tree, ttau).is_none());

        let mut state = get_initial_state(&tree).unwrap();
        println!("{}\n", state);
        assert_eq!(get_enabled_transitions(&tree, &state), [ta]);

        println!("execute a {}", ta);
        execute_transition(&tree, &mut state, ta).unwrap();
        println!("{}\n", state);
        assert_eq!(get_enabled_transitions(&tree, &state), [tf1, tf2]);

        println!("execute f2 {}", tf2);
        execute_transition(&tree, &mut state, tf2).unwrap();
        println!("{}\n", state);
        assert_eq!(get_enabled_transitions(&tree, &state), [td]);

        println!("execute d {}", td);
        execute_transition(&tree, &mut state, td).unwrap();
        println!("{}\n", state);
        assert_eq!(get_enabled_transitions(&tree, &state), [te, ttau]);

        println!("execute e {}", te);
        execute_transition(&tree, &mut state, te).unwrap();
        println!("{}\n", state);
        assert_eq!(get_enabled_transitions(&tree, &state), [td]);

        println!("execute d {}", td);
        execute_transition(&tree, &mut state, td).unwrap();
        println!("{}\n", state);
        assert_eq!(get_enabled_transitions(&tree, &state), [te, ttau]);

        println!("execute tau {}", ttau);
        execute_transition(&tree, &mut state, ttau).unwrap();
        println!("{}\n", state);
        assert_eq!(get_enabled_transitions(&tree, &state), [tfin]);

        println!("terminate {}", tfin);
        execute_transition(&tree, &mut state, tfin).unwrap();
        println!("{}\n", state);
        assert_eq!(get_enabled_transitions(&tree, &state).len(), 0);
    }
}
