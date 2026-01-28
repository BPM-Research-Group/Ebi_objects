use super::process_tree::{Node, Operator};
use crate::{
    Activity, ActivityKey, EbiObject, Exportable, Importable,
    ebi_objects::labelled_petri_net::TransitionIndex,
    line_reader::LineReader,
    traits::{
        graphable,
        importable::{ImporterParameter, ImporterParameterValues, from_string},
    },
};
use anyhow::{Context, Result, anyhow};
use ebi_arithmetic::{Fraction, Signed, Zero};
use ebi_derive::ActivityKey;
use layout::{adt::dag::NodeHandle, topo::layout::VisualGraph};
use std::{
    fmt::Display,
    ops::{Index, IndexMut},
};

pub const HEADER: &str = "stochastic process tree";

#[derive(Debug, ActivityKey, Clone)]
pub struct StochasticProcessTree {
    pub activity_key: ActivityKey,
    pub tree: Vec<Node>,
    pub transition2node: Vec<usize>,
    pub weights: Vec<Fraction>, //weights must be strictly positive; no deadlocks or livelocks in trees. Index are transitions, not nodes.
    pub termination_weight: Fraction,
}

impl StochasticProcessTree {
    pub fn number_of_leaves(&self) -> usize {
        self.tree.iter().filter(|node| node.is_leaf()).count() + 1
    }

    pub fn node_to_string(
        &self,
        indent: usize,
        node: usize,
        f: &mut std::fmt::Formatter<'_>,
    ) -> Result<usize> {
        let id = "\t".repeat(indent);
        match &self.tree[node] {
            Node::Tau => {
                writeln!(
                    f,
                    "{}tau\n{}# weight node {}\n{}{}",
                    id,
                    id,
                    node,
                    id,
                    self.weights[self.node_to_transition(node).unwrap()]
                )?;
                Ok(node + 1)
            }
            Node::Activity(activity) => {
                writeln!(
                    f,
                    "{}activity {}\n{}# weight node {}\n{}{}",
                    id,
                    self.activity_key.get_activity_label(&activity),
                    id,
                    node,
                    id,
                    self.weights[self.node_to_transition(node).unwrap()]
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

    ///read one node, recursively
    fn string_to_tree(
        lreader: &mut LineReader<'_>,
        tree: &mut Vec<Node>,
        weights: &mut Vec<Fraction>,
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
            let weight = lreader.next_line_weight().with_context(|| {
                format!(
                    "failed to read weight of node {} at line {}",
                    tree.len(),
                    lreader.get_last_line_number()
                )
            })?;
            if !weight.is_positive() {
                return Err(anyhow!(
                    "weight of node {} at line {} is not positive",
                    tree.len(),
                    lreader.get_last_line_number()
                ));
            }
            weights.push(weight);
            tree.push(Node::Tau);
        } else if node_type_line.trim_start().starts_with("activity ") {
            let label = node_type_line.trim_start()[9..].to_string();
            let activity = activity_key.process_activity(&label);

            let weight = lreader.next_line_weight().with_context(|| {
                format!(
                    "failed to read weight of node {} at line {}",
                    tree.len(),
                    lreader.get_last_line_number()
                )
            })?;
            if !weight.is_positive() {
                return Err(anyhow!(
                    "weight of node {} at line {} is not positive",
                    tree.len(),
                    lreader.get_last_line_number()
                ));
            }
            weights.push(weight);

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
                Self::string_to_tree(lreader, tree, weights, activity_key, false)?;
            }
        } else if root && node_type_line.trim_start().is_empty() {
            //empty tree
            return Ok(());
        } else {
            return Err(anyhow!(
                "could not parse type of node {} at line {}; Expected `tau`, `activity`, `concurrent`, `interleaved`, `or`, `sequence` or `xor`",
                tree.len(),
                lreader.get_last_line_number()
            ));
        }

        Ok(())
    }

    pub fn node_to_dot(
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
                    child = StochasticProcessTree::node_to_dot(&self, graph, child, entry, exit);
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

                    child = StochasticProcessTree::node_to_dot(
                        &self,
                        graph,
                        child,
                        child_entry,
                        child_exit,
                    );
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
                    child = StochasticProcessTree::node_to_dot(&self, graph, child, &split, &join);
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
                    child = StochasticProcessTree::node_to_dot(&self, graph, child, &split, &join);
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
                    child = StochasticProcessTree::node_to_dot(&self, graph, child, &split, &join);
                }
                child
            }
            Node::Operator(Operator::Loop, number_of_children) => {
                let split = graphable::create_dot(graph);
                graphable::create_edge(graph, entry, &split, "");
                let join = graphable::create_dot(graph);
                graphable::create_edge(graph, &join, exit, "");

                let mut child = node + 1;

                child = StochasticProcessTree::node_to_dot(&self, graph, child, &split, &join);

                if number_of_children == 1 {
                    graphable::create_edge(graph, &join, &split, "");
                } else {
                    for _ in 1..number_of_children {
                        child =
                            StochasticProcessTree::node_to_dot(&self, graph, child, &join, &split);
                    }
                }
                child
            }
        }
    }
}

impl Display for StochasticProcessTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}", HEADER)?;
        if !self.tree.is_empty() {
            let _ = self.node_to_string(0, 0, f);
        };
        writeln!(f, "# termination weight\n{}", self.termination_weight)
    }
}

impl Importable for StochasticProcessTree {
    const FILE_FORMAT_SPECIFICATION_LATEX: &str = "A stochastic process tree is a line-based structure. Lines starting with a \\# are ignored.
    This first line is exactly `stochastic process tree'.
    The subsequent lines contain the nodes:
    Each node is either:
    \\begin{itemize}
        \\item A line with the word `activity' followed on the same line by a space and the label of the activity leaf. The next line contains the weight of the activity;
        \\item The word `tau', followed on the next line by the weight of the leaf;
        \\item The name of an operator (`sequence', `xor', `concurrent', `loop', `interleaved', or `or') on its own line.
        The line thereafter contains the number of children of the node, after which the nodes are given.
        An operator node must have at least one child.
    \\end{itemize}
    Indentation of nodes is allowed, but not mandatory.\\
    The last line of the file contains the weight of termination.
    
    For instance:
    \\lstinputlisting[language=ebilines, style=boxed]{../testfiles/all_operators.sptree}";

    const IMPORTER_PARAMETERS: &[ImporterParameter] = &[];

    fn import_as_object(
        reader: &mut dyn std::io::BufRead,
        parameter_values: &ImporterParameterValues,
    ) -> Result<EbiObject> {
        Ok(EbiObject::StochasticProcessTree(Self::import(
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
        let mut weights = vec![];
        Self::string_to_tree(
            &mut lreader,
            &mut tree,
            &mut weights,
            &mut activity_key,
            true,
        )?;

        let termination_weight = lreader
            .next_line_weight()
            .with_context(|| format!("could not read termination weight at end of file"))?;
        if !termination_weight.is_positive() {
            return Err(anyhow!(
                "termination weight ({}) is not positive",
                termination_weight
            ));
        }

        Ok(StochasticProcessTree::from((
            activity_key,
            tree,
            weights,
            termination_weight,
        )))
    }
}
from_string!(StochasticProcessTree);

impl Exportable for StochasticProcessTree {
    fn export_from_object(object: EbiObject, f: &mut dyn std::io::Write) -> Result<()> {
        match object {
            EbiObject::StochasticProcessTree(lpn) => lpn.export(f),
            _ => Err(anyhow!(
                "cannot export {} {} as a stochastic process tree",
                object.get_type().get_article(),
                object.get_type()
            )),
        }
    }

    fn export(&self, f: &mut dyn std::io::Write) -> Result<()> {
        Ok(write!(f, "{}", self)?)
    }
}

// Semantics belong in the Ebi crate, not here in Ebi_objects,
// however we use them to create a state space in several conversions.
// As such, it is here.
pub fn get_initial_state(tree: &StochasticProcessTree) -> Option<TreeMarking> {
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

pub fn execute_transition(
    tree: &StochasticProcessTree,
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

pub fn is_final_state(_tree: &StochasticProcessTree, state: &TreeMarking) -> bool {
    state.terminated
}

pub fn get_transition_activity(
    tree: &StochasticProcessTree,
    transition: TransitionIndex,
) -> Option<Activity> {
    let node = tree.transition2node.get(transition)?;
    match tree.tree[*node] {
        Node::Tau => None,
        Node::Activity(activity) => Some(activity),
        Node::Operator(_, _) => None,
    }
}

pub fn get_enabled_transitions(
    tree: &StochasticProcessTree,
    state: &TreeMarking,
) -> Vec<TransitionIndex> {
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

pub fn get_number_of_transitions(tree: &StochasticProcessTree) -> usize {
    tree.tree.iter().filter(|node| node.is_leaf()).count() + 1 //the last transition is explicit termination, which is required by the semantics of Ebi
}

/// Starts executing a node.
/// Recurses upwards to adjust enablement.
fn start_node(
    tree: &StochasticProcessTree,
    state: &mut TreeMarking,
    node: usize,
    child: Option<usize>,
) {
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

fn withdraw_enablement(tree: &StochasticProcessTree, state: &mut TreeMarking, node: usize) {
    for grandchild in node..tree.traverse(node) {
        state[grandchild] = NodeState::Closed;
    }
}

fn close_node(tree: &StochasticProcessTree, state: &mut TreeMarking, node: usize) {
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
            Node::Operator(Operator::Concurrent, _) | Node::Operator(Operator::Interleaved, _) => {
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

/// withdraws the enablement of the next sequential nodes
fn withdraw_enablement_next_sequential_nodes(
    tree: &StochasticProcessTree,
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
                    withdraw_enablement(tree, state, tree.get_child(parent, child_rank + 1));
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
                        withdraw_enablement(tree, state, tree.get_child(parent, child_rank));
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

/// enable the nodes that are sequentially next from this node
fn enable_next_sequential_nodes(
    tree: &StochasticProcessTree,
    state: &mut TreeMarking,
    node: usize,
) {
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

fn enable_node(tree: &StochasticProcessTree, state: &mut TreeMarking, node: usize) {
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

pub(crate) fn can_execute(tree: &StochasticProcessTree, state: &TreeMarking, node: usize) -> bool {
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
                if state[child] == NodeState::Started && !can_terminate(tree, state, child) {
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

/**
 * Returns whether it is possible to withdraw the enablement.
 */
fn can_withdraw_enablement(
    _tree: &StochasticProcessTree,
    state: &TreeMarking,
    node: usize,
) -> bool {
    state[node] == NodeState::Enabled
}

/**
 * Returns whether it is possible that this node now terminates (true), or that a leaf has to be executed first (false).
 */
pub(crate) fn can_terminate(
    tree: &StochasticProcessTree,
    state: &TreeMarking,
    node: usize,
) -> bool {
    match tree.tree[node] {
        Node::Tau => state[node] == NodeState::Closed,
        Node::Activity(_) => state[node] == NodeState::Closed,
        Node::Operator(Operator::Concurrent, _) | Node::Operator(Operator::Interleaved, _) => {
            //these nodes can terminate if all of their children are either closed or can terminate
            tree.get_children(node)
                .all(|child| state[child] == NodeState::Closed || can_terminate(tree, state, child))
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
            tree.get_children(node)
                .all(|child| state[child] == NodeState::Closed || can_terminate(tree, state, child))
        }
    }
}

pub fn get_transition_weight<'a>(
    tree: &'a StochasticProcessTree,
    _state: &TreeMarking,
    transition: TransitionIndex,
) -> &'a Fraction {
    if transition < tree.transition2node.len() {
        &tree.weights[transition]
    } else {
        &tree.termination_weight
    }
}

pub fn get_total_weight_of_enabled_transitions(
    tree: &StochasticProcessTree,
    state: &TreeMarking,
) -> Fraction {
    let mut sum = if !state.terminated && can_terminate(tree, state, tree.root()) {
        tree.termination_weight.clone()
    } else {
        Fraction::zero()
    };

    for (transition, node) in tree.transition2node.iter().enumerate() {
        if can_execute(tree, state, *node) {
            sum += &tree.weights[transition];
        }
    }

    sum
}

#[derive(Clone, strum_macros::Display, Debug, Eq, PartialEq, Hash)]
pub enum NodeState {
    Enabled,
    Started,
    Closed,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
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

#[cfg(test)]
mod tests {
    use crate::{
        HasActivityKey, StochasticProcessTree,
        ebi_objects::stochastic_process_tree::{
            can_execute, can_terminate, execute_transition, get_enabled_transitions,
            get_initial_state, get_total_weight_of_enabled_transitions, get_transition_activity,
        },
    };
    use ebi_arithmetic::Fraction;
    use std::fs;

    #[test]
    fn sptree_semantics_loop() {
        let fin1 =
            fs::read_to_string("testfiles/seq(a,xor(seq(f,and(c,b)),seq(f,loop(d,e))).sptree")
                .unwrap();
        let tree = fin1.parse::<StochasticProcessTree>().unwrap();

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
        assert_eq!(
            get_total_weight_of_enabled_transitions(&tree, &state),
            Fraction::from(1)
        );

        println!("execute a {}", ta);
        execute_transition(&tree, &mut state, ta).unwrap();
        println!("{}\n", state);
        assert_eq!(get_enabled_transitions(&tree, &state), [tf1, tf2]);
        assert_eq!(
            get_total_weight_of_enabled_transitions(&tree, &state),
            Fraction::from(4)
        );

        println!("execute f2 {}", tf2);
        execute_transition(&tree, &mut state, tf2).unwrap();
        println!("{}\n", state);
        assert_eq!(get_enabled_transitions(&tree, &state), [td]);

        println!("execute d {}", td);
        execute_transition(&tree, &mut state, td).unwrap();
        println!("{}\n", state);
        assert_eq!(get_enabled_transitions(&tree, &state), [te, ttau]);
        assert_eq!(
            get_total_weight_of_enabled_transitions(&tree, &state),
            Fraction::from(2)
        );

        println!("execute e {}", te);
        execute_transition(&tree, &mut state, te).unwrap();
        println!("{}\n", state);
        assert_eq!(get_enabled_transitions(&tree, &state), [td]);

        println!("execute d {}", td);
        execute_transition(&tree, &mut state, td).unwrap();
        println!("{}\n", state);
        assert_eq!(get_enabled_transitions(&tree, &state), [te, ttau]);
        assert_eq!(
            get_total_weight_of_enabled_transitions(&tree, &state),
            Fraction::from(2)
        );

        println!("execute tau {}", ttau);
        execute_transition(&tree, &mut state, ttau).unwrap();
        println!("{}\n", state);
        assert_eq!(get_enabled_transitions(&tree, &state), [tfin]);
        assert_eq!(
            get_total_weight_of_enabled_transitions(&tree, &state),
            Fraction::from(1)
        );

        println!("terminate {}", tfin);
        execute_transition(&tree, &mut state, tfin).unwrap();
        println!("{}\n", state);
        assert_eq!(get_enabled_transitions(&tree, &state).len(), 0);
        assert_eq!(
            get_total_weight_of_enabled_transitions(&tree, &state),
            Fraction::from(0)
        );
    }

    #[test]
    fn sptree_semantics_concurrent_loop_termination() {
        let fin1 = fs::read_to_string("testfiles/seq(a,and(b,loop(c,d))).sptree").unwrap();
        let tree = fin1.parse::<StochasticProcessTree>().unwrap();

        let ta = 0;
        let tb = 1;
        let tc = 2;
        let td = 3;
        let tfin = 4;

        assert_eq!(
            tree.activity_key()
                .deprocess_activity(&get_transition_activity(&tree, ta).unwrap()),
            "a"
        );
        assert_eq!(
            tree.activity_key()
                .deprocess_activity(&get_transition_activity(&tree, tb).unwrap()),
            "b"
        );
        assert_eq!(
            tree.activity_key()
                .deprocess_activity(&get_transition_activity(&tree, tc).unwrap()),
            "c"
        );
        assert_eq!(
            tree.activity_key()
                .deprocess_activity(&get_transition_activity(&tree, td).unwrap()),
            "d"
        );
        assert!(get_transition_activity(&tree, tfin).is_none());

        let mut state = get_initial_state(&tree).unwrap();
        println!("{}\n", state);
        assert_eq!(get_enabled_transitions(&tree, &state), [ta]);
        assert_eq!(
            get_total_weight_of_enabled_transitions(&tree, &state),
            Fraction::from(1)
        );

        println!("execute a {}", ta);
        execute_transition(&tree, &mut state, ta).unwrap();
        println!("{}\n", state);
        assert_eq!(get_enabled_transitions(&tree, &state), [tb, tc]);
        assert_eq!(
            get_total_weight_of_enabled_transitions(&tree, &state),
            Fraction::from(5)
        );

        println!("execute c {}", tc);
        execute_transition(&tree, &mut state, tc).unwrap();
        println!("{}\n", state);
        assert_eq!(get_enabled_transitions(&tree, &state), [tb, td]);
        assert_eq!(
            get_total_weight_of_enabled_transitions(&tree, &state),
            Fraction::from(6)
        );

        println!("execute b {}", tb);
        execute_transition(&tree, &mut state, tb).unwrap();
        println!("{}\n", state);
        assert_eq!(get_enabled_transitions(&tree, &state), [td, tfin]);
        assert_eq!(
            get_total_weight_of_enabled_transitions(&tree, &state),
            Fraction::from(9)
        );

        println!("execute d {}", td);
        execute_transition(&tree, &mut state, td).unwrap();
        println!("{}\n", state);
        assert_eq!(get_enabled_transitions(&tree, &state), [tc]);
        assert_eq!(
            get_total_weight_of_enabled_transitions(&tree, &state),
            Fraction::from(3)
        );

        println!("execute c {}", tc);
        execute_transition(&tree, &mut state, tc).unwrap();
        println!("{}\n", state);
        assert_eq!(get_enabled_transitions(&tree, &state), [td, tfin]);
        assert_eq!(
            get_total_weight_of_enabled_transitions(&tree, &state),
            Fraction::from(9)
        );

        println!("terminate {}", tfin);
        execute_transition(&tree, &mut state, tfin).unwrap();
        println!("{}\n", state);
        assert_eq!(get_enabled_transitions(&tree, &state).len(), 0);
        assert_eq!(
            get_total_weight_of_enabled_transitions(&tree, &state),
            Fraction::from(0)
        );
    }

    #[test]
    fn sptree_semantics_loop_loop_termination() {
        let fin1 = fs::read_to_string("testfiles/loop(a,loop(b,c)).sptree").unwrap();
        let tree = fin1.parse::<StochasticProcessTree>().unwrap();

        let ta = 0;
        let tb = 1;
        let tc = 2;
        let tfin = 3;

        assert_eq!(
            tree.activity_key()
                .deprocess_activity(&get_transition_activity(&tree, ta).unwrap()),
            "a"
        );
        assert_eq!(
            tree.activity_key()
                .deprocess_activity(&get_transition_activity(&tree, tb).unwrap()),
            "b"
        );
        assert_eq!(
            tree.activity_key()
                .deprocess_activity(&get_transition_activity(&tree, tc).unwrap()),
            "c"
        );
        assert!(get_transition_activity(&tree, tfin).is_none());

        let mut state = get_initial_state(&tree).unwrap();
        println!("{}\n", state);
        assert_eq!(get_enabled_transitions(&tree, &state), [ta]);
        assert_eq!(
            get_total_weight_of_enabled_transitions(&tree, &state),
            Fraction::from(1)
        );

        println!("execute a {}", ta);
        execute_transition(&tree, &mut state, ta).unwrap();
        println!("{}\n", state);
        assert_eq!(get_enabled_transitions(&tree, &state), [tb, tfin]);
        assert_eq!(
            get_total_weight_of_enabled_transitions(&tree, &state),
            Fraction::from(6)
        );

        println!("execute b {}", tb);
        execute_transition(&tree, &mut state, tb).unwrap();
        println!("{}\n", state);
        assert_eq!(get_enabled_transitions(&tree, &state), [ta, tc]);
        assert_eq!(
            get_total_weight_of_enabled_transitions(&tree, &state),
            Fraction::from(4)
        );

        println!("execute a {}", ta);
        execute_transition(&tree, &mut state, ta).unwrap();
        println!("{}\n", state);
        assert_eq!(get_enabled_transitions(&tree, &state), [tb, tfin]);
        assert_eq!(
            get_total_weight_of_enabled_transitions(&tree, &state),
            Fraction::from(6)
        );

        println!("execute b {}", tb);
        execute_transition(&tree, &mut state, tb).unwrap();
        println!("{}\n", state);
        assert_eq!(get_enabled_transitions(&tree, &state), [ta, tc]);
        assert_eq!(
            get_total_weight_of_enabled_transitions(&tree, &state),
            Fraction::from(4)
        );

        println!("execute c {}", tc);
        execute_transition(&tree, &mut state, tc).unwrap();
        println!("{}\n", state);
        assert_eq!(get_enabled_transitions(&tree, &state), [tb]);
        assert_eq!(
            get_total_weight_of_enabled_transitions(&tree, &state),
            Fraction::from(2)
        );

        println!("execute b {}", tb);
        execute_transition(&tree, &mut state, tb).unwrap();
        println!("{}\n", state);
        assert_eq!(get_enabled_transitions(&tree, &state), [ta, tc]);
        assert_eq!(
            get_total_weight_of_enabled_transitions(&tree, &state),
            Fraction::from(4)
        );

        println!("execute a {}", ta);
        execute_transition(&tree, &mut state, ta).unwrap();
        println!("{}\n", state);
        assert_eq!(get_enabled_transitions(&tree, &state), [tb, tfin]);
        assert_eq!(
            get_total_weight_of_enabled_transitions(&tree, &state),
            Fraction::from(6)
        );

        println!("terminate {}", tfin);
        execute_transition(&tree, &mut state, tfin).unwrap();
        println!("{}\n", state);
        assert_eq!(get_enabled_transitions(&tree, &state).len(), 0);
        assert_eq!(
            get_total_weight_of_enabled_transitions(&tree, &state),
            Fraction::from(0)
        );
    }

    #[test]
    fn sptree_semantics_loop_right_loop_termination() {
        let fin1 = fs::read_to_string("testfiles/loop(loop(a,b),c).sptree").unwrap();
        let tree = fin1.parse::<StochasticProcessTree>().unwrap();

        let ta = 0;
        let tb = 1;
        let tc = 2;
        let tfin = 3;

        assert_eq!(
            tree.activity_key()
                .deprocess_activity(&get_transition_activity(&tree, ta).unwrap()),
            "a"
        );
        assert_eq!(
            tree.activity_key()
                .deprocess_activity(&get_transition_activity(&tree, tb).unwrap()),
            "b"
        );
        assert_eq!(
            tree.activity_key()
                .deprocess_activity(&get_transition_activity(&tree, tc).unwrap()),
            "c"
        );
        assert!(get_transition_activity(&tree, tfin).is_none());

        let mut state = get_initial_state(&tree).unwrap();
        println!("{}\n", state);
        assert_eq!(get_enabled_transitions(&tree, &state), [ta]);
        assert_eq!(
            get_total_weight_of_enabled_transitions(&tree, &state),
            Fraction::from(1)
        );

        println!("execute a {}", ta);
        execute_transition(&tree, &mut state, ta).unwrap();
        println!("{}\n", state);
        assert_eq!(get_enabled_transitions(&tree, &state), [tb, tc, tfin]);
        assert_eq!(
            get_total_weight_of_enabled_transitions(&tree, &state),
            Fraction::from(9)
        );

        println!("execute c {}", tc);
        execute_transition(&tree, &mut state, tc).unwrap();
        println!("{}\n", state);
        assert_eq!(get_enabled_transitions(&tree, &state), [ta]);
        assert_eq!(
            get_total_weight_of_enabled_transitions(&tree, &state),
            Fraction::from(1)
        );

        println!("execute a {}", ta);
        execute_transition(&tree, &mut state, ta).unwrap();
        println!("{}\n", state);
        assert_eq!(get_enabled_transitions(&tree, &state), [tb, tc, tfin]);
        assert_eq!(
            get_total_weight_of_enabled_transitions(&tree, &state),
            Fraction::from(9)
        );

        println!("execute b {}", tb);
        execute_transition(&tree, &mut state, tb).unwrap();
        println!("{}\n", state);
        assert_eq!(get_enabled_transitions(&tree, &state), [ta]);
        assert_eq!(
            get_total_weight_of_enabled_transitions(&tree, &state),
            Fraction::from(1)
        );

        println!("execute a {}", ta);
        execute_transition(&tree, &mut state, ta).unwrap();
        println!("{}\n", state);
        assert_eq!(get_enabled_transitions(&tree, &state), [tb, tc, tfin]);
        assert_eq!(
            get_total_weight_of_enabled_transitions(&tree, &state),
            Fraction::from(9)
        );

        println!("terminate {}", tfin);
        execute_transition(&tree, &mut state, tfin).unwrap();
        println!("{}\n", state);
        assert_eq!(get_enabled_transitions(&tree, &state).len(), 0);
        assert_eq!(
            get_total_weight_of_enabled_transitions(&tree, &state),
            Fraction::from(0)
        );
    }

    macro_rules! assert_execute_expect {
        ($tree:ident, $state:ident, $t:ident, $e:expr) => {
            println!("execute {} {}", ::std::stringify!($t), $t);
            assert!(can_execute(&$tree, &$state, $tree.transition2node[$t]));
            execute_transition(&$tree, &mut $state, $t).unwrap();
            println!("{}\n", $state);
            assert_eq!(get_enabled_transitions(&$tree, &$state), $e);
        };
    }

    macro_rules! assert_terminate {
        ($tree:ident, $state:ident, $tfin:ident) => {
            println!("terminate {}", $tfin);
            assert!(can_terminate(&$tree, &$state, $tree.root()));
            execute_transition(&$tree, &mut $state, $tfin).unwrap();
            println!("{}\n", $state);
            assert_eq!(get_enabled_transitions(&$tree, &$state).len(), 0);
            assert_eq!(
                get_total_weight_of_enabled_transitions(&$tree, &$state),
                Fraction::from(0)
            );
        };
    }

    #[test]
    fn sptree_semantics_xor_loop_termination() {
        let fin1 = fs::read_to_string("testfiles/loop(xor(a,loop(b,c),d).sptree").unwrap();
        let tree = fin1.parse::<StochasticProcessTree>().unwrap();

        let ta = 0;
        let tb = 1;
        let tc = 2;
        let td = 3;
        let tfin = 4;

        assert_eq!(
            tree.activity_key()
                .deprocess_activity(&get_transition_activity(&tree, ta).unwrap()),
            "a"
        );
        assert_eq!(
            tree.activity_key()
                .deprocess_activity(&get_transition_activity(&tree, tb).unwrap()),
            "b"
        );
        assert_eq!(
            tree.activity_key()
                .deprocess_activity(&get_transition_activity(&tree, tc).unwrap()),
            "c"
        );
        assert_eq!(
            tree.activity_key()
                .deprocess_activity(&get_transition_activity(&tree, td).unwrap()),
            "d"
        );
        assert!(get_transition_activity(&tree, tfin).is_none());

        let mut state = get_initial_state(&tree).unwrap();
        println!("{}\n", state);
        assert_eq!(get_enabled_transitions(&tree, &state), [ta, tb]);

        assert_execute_expect!(tree, state, ta, [td, tfin]);

        assert_execute_expect!(tree, state, td, [ta, tb]);

        assert_execute_expect!(tree, state, tb, [tc, td, tfin]);

        assert_execute_expect!(tree, state, td, [ta, tb]);

        assert_execute_expect!(tree, state, ta, [td, tfin]);

        assert_execute_expect!(tree, state, td, [ta, tb]); //reset

        assert_execute_expect!(tree, state, tb, [tc, td, tfin]);

        assert_execute_expect!(tree, state, tc, [tb]);

        assert_execute_expect!(tree, state, tb, [tc, td, tfin]);

        assert_terminate!(tree, state, tfin);
    }

    #[test]
    fn sptree_semantics_or_loop_termination() {
        let fin1 = fs::read_to_string("testfiles/loop(or(a,loop(b,c),d).sptree").unwrap();
        let tree = fin1.parse::<StochasticProcessTree>().unwrap();

        let ta = 0;
        let tb = 1;
        let tc = 2;
        let td = 3;
        let tfin = 4;

        assert_eq!(
            tree.activity_key()
                .deprocess_activity(&get_transition_activity(&tree, ta).unwrap()),
            "a"
        );
        assert_eq!(
            tree.activity_key()
                .deprocess_activity(&get_transition_activity(&tree, tb).unwrap()),
            "b"
        );
        assert_eq!(
            tree.activity_key()
                .deprocess_activity(&get_transition_activity(&tree, tc).unwrap()),
            "c"
        );
        assert_eq!(
            tree.activity_key()
                .deprocess_activity(&get_transition_activity(&tree, td).unwrap()),
            "d"
        );
        assert!(get_transition_activity(&tree, tfin).is_none());

        let mut state = get_initial_state(&tree).unwrap();
        println!("{}\n", state);
        assert_eq!(get_enabled_transitions(&tree, &state), [ta, tb]);

        assert_execute_expect!(tree, state, ta, [tb, td, tfin]);

        assert_execute_expect!(tree, state, td, [ta, tb]);

        assert_execute_expect!(tree, state, ta, [tb, td, tfin]);

        assert_execute_expect!(tree, state, tb, [tc, td, tfin]);

        assert_execute_expect!(tree, state, tc, [tb]);

        assert_execute_expect!(tree, state, tb, [tc, td, tfin]);

        assert_execute_expect!(tree, state, td, [ta, tb]);

        assert_execute_expect!(tree, state, tb, [ta, tc, td, tfin]);

        assert_execute_expect!(tree, state, tc, [ta, tb]);

        assert_execute_expect!(tree, state, tb, [ta, tc, td, tfin]);

        assert_execute_expect!(tree, state, ta, [tc, td, tfin]);

        assert_terminate!(tree, state, tfin);
    }

    #[test]
    fn sptree_semantics_interleaved_loop_termination() {
        let fin1 =
            fs::read_to_string("testfiles/loop(interleaved(seq(a,b),loop(c,d),e).sptree").unwrap();
        let tree = fin1.parse::<StochasticProcessTree>().unwrap();

        let ta = 0;
        let tb = 1;
        let tc = 2;
        let td = 3;
        let te = 4;
        let tfin = 5;

        assert_eq!(
            tree.activity_key()
                .deprocess_activity(&get_transition_activity(&tree, ta).unwrap()),
            "a"
        );
        assert_eq!(
            tree.activity_key()
                .deprocess_activity(&get_transition_activity(&tree, tb).unwrap()),
            "b"
        );
        assert_eq!(
            tree.activity_key()
                .deprocess_activity(&get_transition_activity(&tree, tc).unwrap()),
            "c"
        );
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
        assert!(get_transition_activity(&tree, tfin).is_none());

        let mut state = get_initial_state(&tree).unwrap();
        println!("{}\n", state);
        assert_eq!(get_enabled_transitions(&tree, &state), [ta, tc]);

        assert_execute_expect!(tree, state, ta, [tb]);

        assert_execute_expect!(tree, state, tb, [tc]);

        assert_execute_expect!(tree, state, tc, [td, te, tfin]);

        assert_execute_expect!(tree, state, td, [tc]);

        assert_execute_expect!(tree, state, tc, [td, te, tfin]);

        assert_execute_expect!(tree, state, te, [ta, tc]); //reset

        assert_execute_expect!(tree, state, tc, [ta, td]);

        assert_execute_expect!(tree, state, td, [tc]);

        assert_execute_expect!(tree, state, tc, [td, te, tfin]);

        assert_execute_expect!(tree, state, td, [tc]);

        assert_execute_expect!(tree, state, tc, [td, te, tfin]);

        assert_terminate!(tree, state, tfin);
    }
}
