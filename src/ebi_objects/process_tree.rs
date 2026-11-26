use crate::{Activity, ActivityKey, HasActivityKey};
#[cfg(any(test, feature = "testactivities"))]
use ebi_activity_key::TestActivityKey;
use ebi_bpmn::ebi_arithmetic::anyhow::{Result, anyhow};
use ebi_derive::ActivityKey;
use itertools::Itertools;
use std::{fmt::Display, sync::OnceLock};
use strum_macros::EnumIter;

/// The string that must be present as the first non-comment line of a process tree file.
pub const HEADER: &str = "process tree";

/// A process tree is a block-structured process model.
///
/// Its implementation keeps a list of nodes.
/// A tree is valid if every operator node has at least one child, and there is only one node without parent.
#[derive(Debug, ActivityKey, Clone, Default)]
pub struct ProcessTree {
    activity_key: ActivityKey,
    tree: Vec<TreeNode>,

    /// Transition -> node
    cache_transition_2_node: OnceLock<Vec<usize>>,

    /// Node -> next node after last child
    cache_traverse: OnceLock<Vec<usize>>,

    /// Cache of the parent of a node, and the child-rank of the node.
    cache_node_2_parent: OnceLock<Vec<(usize, usize)>>,
}

impl ProcessTree {
    /// Create a new process tree with the given activity key and node array.
    ///
    /// It is the responsibility of the caller to ensure the tree is valid.
    /// That is, an operator must have >= 1 children, and there must not be multiple roots.
    pub fn new(activity_key: ActivityKey, tree: Vec<TreeNode>) -> Self {
        Self {
            activity_key,
            tree,
            cache_traverse: OnceLock::new(),
            cache_transition_2_node: OnceLock::new(),
            cache_node_2_parent: OnceLock::new(),
        }
    }

    /// Create a new process tree without any nodes (having the empty language).
    pub fn new_with_activity_key(activity_key: ActivityKey) -> ProcessTree {
        Self {
            activity_key,
            tree: vec![],
            cache_traverse: OnceLock::new(),
            cache_transition_2_node: OnceLock::new(),
            cache_node_2_parent: OnceLock::new(),
        }
    }

    /// Deconstruct the tree by taking its internal components.
    pub(crate) fn take(self) -> (ActivityKey, Vec<TreeNode>) {
        (self.activity_key, self.tree)
    }

    /// Returns a string representation of the tree that can be used to compare trees with one another.
    pub fn to_canonical_string(&self) -> String {
        if self.tree.is_empty() {
            String::new()
        } else {
            self.node_to_canonical_string(0)
        }
    }

    fn node_to_canonical_string(&self, node: usize) -> String {
        match self.tree[node] {
            TreeNode::Tau => "tau".to_string(),
            TreeNode::Activity(activity) => {
                let label = self.activity_key.deprocess_activity(&activity);
                format!("activity {} {}", label.len(), label)
            }
            TreeNode::Operator(TreeOperator::Concurrent, _)
            | TreeNode::Operator(TreeOperator::Xor, _)
            | TreeNode::Operator(TreeOperator::Or, _)
            | TreeNode::Operator(TreeOperator::Interleaved, _) => format!(
                "node {:?} [{}]",
                self.node_to_operator(node).unwrap(),
                self.children(node)
                    .map(|child| self.node_to_canonical_string(child))
                    .sorted()
                    .join(", ")
            ),
            TreeNode::Operator(TreeOperator::Sequence, _) => format!(
                "node sequence [{}]",
                self.children(node)
                    .map(|child| self.node_to_canonical_string(child))
                    .join(", ")
            ),
            TreeNode::Operator(TreeOperator::Loop, _) => {
                format!(
                    "node loop {} [{}]",
                    self.node_to_canonical_string(node + 1),
                    self.children(node)
                        .skip(1)
                        .map(|child| self.node_to_canonical_string(child))
                        .sorted()
                        .join(", ")
                )
            }
        }
    }
}

macro_rules! tree {
    ($t:ident, $u:ident, $v:ident) => {
        impl $t {
            /// Returns the number of nodes in the tree.
            pub fn len(&self) -> usize {
                self.tree.len()
            }

            /// Returns whether the tree has any nodes.
            pub fn is_empty(&self) -> bool {
                self.tree.is_empty()
            }

            /// Returns the node if it exists.
            pub fn get(&self, node: usize) -> Option<&$crate::TreeNode> {
                self.tree.get(node)
            }

            /// Returns the root of the tree (the root does not exist if the tree is empty).
            pub fn root(&self) -> usize {
                0
            }

            /// Returns an array with node indices of the leaves of the process tree, in the order they are encountered in the tree.
            pub fn leaves(&self) -> &Vec<usize> {
                self.cache_transition_2_node
                    .get_or_init(|| self.compute_transition_to_node())
            }

            /// Returns the number of tau and activity nodes in the tree.
            pub fn number_of_leaves(&self) -> usize {
                self.cache_transition_2_node
                    .get_or_init(|| self.compute_transition_to_node())
                    .len()
            }

            /// Returns the operator of the node, or None if the node is not an operator.
            pub fn node_to_operator(&self, node: usize) -> Option<$crate::TreeOperator> {
                match self.tree.get(node)? {
                    $crate::TreeNode::Tau | $crate::TreeNode::Activity(_) => None,
                    $crate::TreeNode::Operator(operator, _) => Some(*operator),
                }
            }

            /// Given a transition, returns the node and its index.
            pub fn transition_to_node(&self, transition: $crate::TransitionIndex) -> Option<usize> {
                self.cache_transition_2_node
                    .get_or_init(|| Self::compute_transition_to_node(self))
                    .get(transition)
                    .copied()
            }

            fn compute_transition_to_node(&self) -> Vec<usize> {
                let mut result = vec![];
                for (node_index, node) in self.tree.iter().enumerate() {
                    match node {
                        $crate::TreeNode::Tau | $crate::TreeNode::Activity(_) => {
                            result.push(node_index);
                        }
                        $crate::TreeNode::Operator(_, _) => {}
                    }
                }
                result
            }

            /// Returns the parent of node, and the rank of the node.
            /// Returns None if the parent or the node does not exist.
            pub fn parent(&self, node: usize) -> Option<(usize, usize)> {
                if node == 0 {
                    return None;
                }

                let arr = self.cache_node_2_parent.get_or_init(|| {
                    let mut parents = vec![(0, 0); self.len()];
                    let mut stack: Vec<(usize, usize, usize)> = Vec::new();

                    for (i, node) in self.iter().enumerate() {
                        if let Some(top) = stack.last_mut() {
                            parents[i] = (top.0, top.2);
                            top.1 -= 1; // top always has remaining > 0: finished entries are popped below
                            top.2 += 1;
                        }
                        if let $crate::TreeNode::Operator(_, n) = node {
                            if *n > 0 {
                                stack.push((i, *n, 0));
                            }
                        }
                        while matches!(stack.last(), Some(&(_, 0, _))) {
                            stack.pop();
                        }
                    }
                    parents
                });

                arr.get(node).copied()
            }

            /// Returns the rank of the child within parent that contains grandChild.
            /// If grandChild is not a child of parent, will return None.
            pub fn get_child_rank_with(&self, parent: usize, grand_child: usize) -> Option<usize> {
                let mut child_rank = 0;
                for child in self.children(parent) {
                    if self.is_parent_of(child, grand_child) {
                        return Some(child_rank);
                    }
                    child_rank += 1;
                }
                None
            }

            /// Returns an iterator over the direct children of this node.
            pub fn children(&self, node: usize) -> $u<'_> {
                $u::new(self, node)
            }

            /// Returns an iterator over the (grand) parents of this node.
            pub fn ancestors(&self, child: usize) -> $v<'_> {
                $v::new(self, child)
            }

            /// Returns all nodes of which `node` is a (grand) parent.
            /// Does not return node itself.
            pub fn get_descendants(&self, node: usize) -> &[$crate::TreeNode] {
                if self[node].is_leaf() {
                    return &[];
                }
                let next = self.traverse(node);
                &self.tree[node + 1..next]
            }

            /// Returns whether the child is a direct or indirect child of parent.
            /// Parent = child also returns `true`.
            pub fn is_parent_of(&self, parent: usize, child: usize) -> bool {
                if parent > child {
                    return false;
                }
                return self.traverse(parent) > child;
            }

            /**
             * Find the next node after the subtere rooted in node.
             * May return a non-existing node if there is no sibling.
             */
            pub fn traverse(&self, node: usize) -> usize {
                self.cache_traverse.get_or_init(|| self.compute_traverse())[node]
            }

            fn compute_traverse(&self) -> Vec<usize> {
                let len = self.len();
                let mut next = vec![0usize; len];

                for i in (0..len).rev() {
                    next[i] = match &self[i] {
                        $crate::TreeNode::Tau | $crate::TreeNode::Activity(_) => i + 1,
                        $crate::TreeNode::Operator(_, number_of_children) => {
                            let mut child = i + 1;
                            for _ in 0..*number_of_children {
                                child = next.get(child).copied().unwrap_or(len);
                            }
                            child
                        }
                    };
                }
                next
            }

            /// Returns the child with rank `child_rank` of node `parent`.
            /// Does not verify whether the node is an operator, or whether `child_rank` <= its number of children.
            pub fn get_child(&self, parent: usize, child_rank: usize) -> usize {
                let mut i = parent + 1;
                for _ in 0..child_rank {
                    i = self.traverse(i);
                }
                return i;
            }

            /// If the node is an operator, set it to the given operator type.
            /// Otherwise, has no effect.
            pub fn set_operator(&mut self, node: usize, operator: $crate::TreeOperator) {
                if let Some($crate::TreeNode::Operator(op, _)) = self.tree.get_mut(node) {
                    *op = operator;
                    self.clear_caches();
                }
            }

            /// Returns the transition that belongs to a given node, or None if the node does not have a transition or does not exist.
            pub fn node_to_transition(&self, node: usize) -> Option<usize> {
                let transition_2_node = self
                    .cache_transition_2_node
                    .get_or_init(|| self.compute_transition_to_node());
                transition_2_node.binary_search(&node).ok()
            }

            /**
             * Removes a node from the tree.
             * If the node has children, these will be moved to the parent of the removed node (return an error if there is no such parent.).
             * If the parent has no children left, it will be removed as well.
             * Returns an error if the node does not exist.
             * If an error is returned, the tree may be left in an inconsistent state.
             */
            pub fn remove(&mut self, node: usize) -> $crate::anyhow::Result<()> {
                self.remove_internal(node)?;
                self.clear_caches();
                Ok(())
            }

            pub fn iter(&self) -> impl Iterator<Item = &$crate::TreeNode> {
                self.tree.iter()
            }
        }

        impl std::ops::Index<usize> for $t {
            type Output = $crate::TreeNode;

            fn index(&self, index: usize) -> &Self::Output {
                &self.tree[index]
            }
        }

        impl std::ops::Index<core::ops::Range<usize>> for $t {
            type Output = [$crate::TreeNode];

            fn index(&self, index: core::ops::Range<usize>) -> &Self::Output {
                self.tree.index(index)
            }
        }

        impl $crate::TranslateActivityKey for $t {
            fn translate_using_activity_key(&mut self, to_activity_key: &mut $crate::ActivityKey) {
                let translator =
                    $crate::ActivityKeyTranslator::new(&self.activity_key, to_activity_key);
                self.tree.iter_mut().for_each(|node| {
                    if let $crate::TreeNode::Activity(a) = node {
                        *a = translator.translate_activity(&a)
                    }
                });
                self.activity_key = to_activity_key.clone();
            }
        }

        impl $crate::Infoable for $t {
            fn info(&self, f: &mut impl std::io::Write) -> $crate::anyhow::Result<()> {
                writeln!(f, "Number of nodes\t\t{}", self.len())?;
                writeln!(
                    f,
                    "Number of activities\t\t{}",
                    $t::activity_key(self).len()
                )?;

                writeln!(f, "")?;
                self.activity_key().info(f)?;

                Ok(writeln!(f, "")?)
            }
        }

        impl<'a> IntoIterator for &'a $t {
            type Item = &'a $crate::TreeNode;
            type IntoIter = core::slice::Iter<'a, $crate::TreeNode>;

            /// Iterate over the nodes of the tree.
            fn into_iter(self) -> Self::IntoIter {
                self.tree.iter()
            }
        }

        /// Iterate over the children of a node.
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
                    tree,
                    node,
                    now: None,
                    next: node + 1,
                    count: 0,
                }
            }
        }

        impl<'a> Iterator for $u<'a> {
            type Item = usize;

            fn next(&mut self) -> Option<Self::Item> {
                if self.count >= self.tree.get(self.node)?.number_of_children() {
                    return None;
                }
                self.count += 1;
                self.now = Some(self.next);
                self.next = self.tree.traverse(self.now.unwrap());
                Some(self.now.unwrap())
            }
        }

        /// Iterate over the parents of a node.
        pub struct $v<'a> {
            //parents iterator
            tree: &'a $t,
            node: Option<(usize, usize)>,
        }

        impl<'a> $v<'a> {
            fn new(tree: &'a $t, node: usize) -> Self {
                Self {
                    tree,
                    node: tree.parent(node),
                }
            }
        }

        impl<'a> Iterator for $v<'a> {
            type Item = (usize, usize);

            fn next(&mut self) -> Option<Self::Item> {
                if let Some((node, child_rank)) = self.node {
                    self.node = self.tree.parent(node);

                    Some((node, child_rank))
                } else {
                    None
                }
            }
        }
    };
}
pub(crate) use tree;
tree!(ProcessTree, ChildrenIterator, ParentsIterator);

impl PartialEq for ProcessTree {
    fn eq(&self, other: &Self) -> bool {
        self.to_canonical_string() == other.to_canonical_string()
    }
}
impl Eq for ProcessTree {}

/// A node in a process tree.
#[derive(Debug, Clone, Copy)]
pub enum TreeNode {
    /// A node with the empty language (silent leaf).
    Tau,

    /// A node that expresses the execution of an activity (activity leaf).
    Activity(Activity),

    /// An operator node.
    /// Contains the operator type and the number of children (which must be >= 1).
    Operator(TreeOperator, usize),
}

impl TreeNode {
    pub fn is_leaf(&self) -> bool {
        match self {
            Self::Tau | Self::Activity(_) => true,
            Self::Operator(_, _) => false,
        }
    }

    pub fn is_activity(&self) -> bool {
        matches!(self, Self::Activity(_))
    }

    pub fn is_tau(&self) -> bool {
        matches!(self, Self::Tau)
    }

    /// Sets the number of children of the node.
    /// Returns an error if `number_of_children` is zero or if the node is not an operator node.
    pub fn set_number_of_children(&mut self, number_of_children: usize) -> Result<()> {
        if let Self::Operator(_, old_number_of_children) = self {
            if number_of_children == 0 {
                return Err(anyhow!("Cannot set a node to zero children."));
            }
            *old_number_of_children = number_of_children;
            Ok(())
        } else {
            Err(anyhow!(
                "Attempted to alter the number of children of a leaf (activity or a tau)."
            ))
        }
    }

    /// Sets the number of children.
    /// Allows `number_of_children`, and does not have any effect if the node is not an operator node.
    pub(super) fn set_number_of_children_unchecked(&mut self, number_of_children: usize) {
        if let Self::Operator(_, old_number_of_children) = self {
            *old_number_of_children = number_of_children;
        }
    }

    pub fn number_of_children(&self) -> usize {
        match self {
            TreeNode::Tau => 0,
            TreeNode::Activity(_) => 0,
            TreeNode::Operator(_, number_of_children) => *number_of_children,
        }
    }

    pub fn is_operator_of_type(&self, operator: TreeOperator) -> bool {
        match self {
            TreeNode::Tau => false,
            TreeNode::Activity(_) => false,
            TreeNode::Operator(op, _) => *op == operator,
        }
    }

    pub fn is_operator_and_matches(&self, other: &TreeNode) -> bool {
        match (self, other) {
            (TreeNode::Tau, _) => false,
            (TreeNode::Activity(_), _) => false,
            (_, TreeNode::Tau) => false,
            (_, TreeNode::Activity(_)) => false,
            (TreeNode::Operator(op, _), TreeNode::Operator(op2, _)) => op == op2,
        }
    }

    pub fn is_operator_xor(&self) -> bool {
        matches!(self, TreeNode::Operator(TreeOperator::Xor, _))
    }

    pub fn is_operator_or(&self) -> bool {
        matches!(self, TreeNode::Operator(TreeOperator::Or, _))
    }

    pub fn is_operator_interleaved(&self) -> bool {
        matches!(self, TreeNode::Operator(TreeOperator::Interleaved, _))
    }

    pub fn is_operator_concurrent(&self) -> bool {
        matches!(self, TreeNode::Operator(TreeOperator::Concurrent, _))
    }

    pub fn is_operator_loop(&self) -> bool {
        matches!(self, TreeNode::Operator(TreeOperator::Loop, _))
    }

    pub fn is_operator_sequence(&self) -> bool {
        matches!(self, TreeNode::Operator(TreeOperator::Sequence, _))
    }
}

/// The type of operator of a node in a process tree.
#[derive(EnumIter, Debug, Clone, Copy, PartialEq, Eq)]
pub enum TreeOperator {
    /// Exclusive choice. On execution, allows for one child to execute.
    Xor,

    /// Sequence. On execution, every child executes in order.
    Sequence,

    /// Interleaved.
    ///
    /// On execution, executes every child, in no particular, though non-overlapping, order.
    Interleaved,

    /// Concurrent.
    ///
    /// On execution, executes all children concurrently.
    Concurrent,

    /// Inclusive choice.
    ///
    /// On execution, executes at least one child, and other children optionally, concurrently.
    Or,

    /// Loop.
    ///
    /// On execution, executes its first child. Then, there is a choice between any non-first children followed by another execution of the first child.
    /// If there is only a single child, then the loop behaves as if its second child was a tau.
    Loop,
}

impl Display for TreeOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                TreeOperator::Xor => "xor",
                TreeOperator::Sequence => "sequence",
                TreeOperator::Interleaved => "interleaved",
                TreeOperator::Concurrent => "concurrent",
                TreeOperator::Or => "or",
                TreeOperator::Loop => "loop",
            }
        )
    }
}

pub mod edit {
    use crate::{
        ProcessTree, TreeNode,
        anyhow::{Result, anyhow},
    };
    use itertools::Itertools;

    impl ProcessTree {
        /// Change the process tree in a closure.
        /// It is the responsibility of the caller to ensure that the tree remains valid.
        /// That is, an operator must have >= 1 children, and there must not be multiple roots.
        pub fn edit(&mut self, closure: impl FnOnce(&mut Vec<TreeNode>)) {
            closure(&mut self.tree);
            self.clear_caches();
        }

        /// Removes and returns the node and all of its children from the tree.
        /// If the parent of the node has no children left, it is removed (but not returned).
        /// Returns an error if the node does not exist.
        ///
        /// Note that if the first child of a loop is removed, the second child moves to the first position, which makes it a body rather than a redo.
        pub fn remove_recursive(&mut self, node: usize) -> Result<Vec<TreeNode>> {
            if node >= self.len() {
                return Err(anyhow!("Node {node} does not exist."));
            }

            let node_after_child_tree = self.traverse(node);
            self.parent(1); //ensure the parent cache is filled
            let removed = self.tree.drain(node..node_after_child_tree).collect_vec();

            //Invariant: the cache is valid up till node.

            let mut target = node;
            while let Some((parent, _)) = self.parent(target) {
                let number_of_children = self[parent].number_of_children();
                self.tree[parent].set_number_of_children_unchecked(number_of_children - 1);
                if number_of_children > 1 {
                    break;
                }
                self.tree.remove(parent);

                target = parent;
            }
            self.clear_caches();
            Ok(removed)
        }

        /// Remove a node from the tree.
        ///
        /// It is the responsibility of the caller to clear the caches after calling this function.
        pub(crate) fn remove_internal_do(&mut self, node: usize) {
            self.tree.remove(node);
        }

        /// Removes and returns the node from the tree (recursively) and replace it with another single node (recursively).
        /// Returns an error if the node does not exist.
        /// Returns the removed node (recursively) if successful.
        ///
        /// It is the responsibility of the caller to ensure the `with` parameter contains a valid tree.
        /// That is, an operator must have >= 1 children, and there must not be multiple roots.
        /// Providing an invalid `with` will result in an invalid tree.
        pub fn replace<W>(&mut self, node: usize, with: W) -> Result<Vec<TreeNode>>
        where
            W: IntoIterator<Item = TreeNode>,
        {
            if node >= self.len() {
                return Err(anyhow!("Node {node} does not exist."));
            }
            let subtree_ends = self.traverse(node);
            let result = self.tree.splice(node..subtree_ends, with).collect_vec();
            self.clear_caches();
            Ok(result)
        }
    }

    macro_rules! tree_edit {
        ($t:tt) => {
            impl $t {
                /// Perform the removal of a node without clearing caches.
                /// It is the responsibility of the caller to clear the caches after calling this method.
                pub(super) fn remove_internal(
                    &mut self,
                    node: usize,
                ) -> $crate::anyhow::Result<()> {
                    if node >= self.len() {
                        return Err($crate::anyhow::anyhow!("Node {node} does not exist."));
                    }

                    if let Some((parent, _)) = self.parent(node) {
                        //node had a parent
                        let children_of_node = self[node].number_of_children();
                        let children_of_parent = self[parent].number_of_children();
                        self.remove_internal_do(node);
                        match (children_of_node, children_of_parent) {
                            (_, 0) => return Err($crate::anyhow::anyhow!("Invalid tree.")),
                            (0, 1) => {
                                //p(n) -> p() -> recurse
                                //parent won't have any children left; recurse
                                self.tree[parent].set_number_of_children_unchecked(0);
                                self.remove_internal(parent)?;
                            }
                            (0, _) => {
                                //p(n, ..) -> p(..)
                                self.tree[parent]
                                    .set_number_of_children_unchecked(children_of_parent - 1);
                            }
                            (1, _) => {
                                //p(n(c), ..) -> p(c, ..)
                            }
                            (_, 1) => {
                                //p(n(c, ..)) -> p(c, ..)
                                self.tree[parent]
                                    .set_number_of_children_unchecked(children_of_node);
                            }
                            (_, _) => {
                                //p(n(c, ..), ..) -> p(c, .., ..)
                                self.tree[parent].set_number_of_children_unchecked(
                                    children_of_parent + children_of_node - 1,
                                );
                            }
                        }
                    } else {
                        //node is the root
                        if self[node].number_of_children() > 1 {
                            return Err($crate::anyhow::anyhow!(
                                "Cannot remove the root as it has multiple children."
                            ));
                        }
                        self.remove_internal_do(node);
                    };

                    Ok(())
                }

                /// Clears caches. Rather cheap operation; caches are re-filled when requested.
                pub(crate) fn clear_caches(&mut self) {
                    self.cache_traverse = std::sync::OnceLock::new();
                    self.cache_transition_2_node = std::sync::OnceLock::new();
                    self.cache_node_2_parent = std::sync::OnceLock::new();
                }
            }
        };
    }
    pub(crate) use tree_edit;
    tree_edit!(ProcessTree);
}

pub(crate) mod graph {
    use crate::{ProcessTree, TreeNode, TreeOperator, traits::graphable};
    use layout::adt::dag::NodeHandle;
    use strum_macros::EnumIs;

    macro_rules! tree_graphable {
        ($t:tt) => {
            impl $crate::Graphable for $t {
                fn to_dot(&self) -> $crate::anyhow::Result<layout::topo::layout::VisualGraph> {
                    let mut graph = layout::topo::layout::VisualGraph::new(
                        layout::core::base::Orientation::LeftToRight,
                    );
                    let source = $crate::ebi_objects::process_tree::graph::Gateway::Xor(
                        $crate::traits::graphable::create_place(&mut graph, ""),
                    );
                    let sink = $crate::ebi_objects::process_tree::graph::Gateway::Xor(
                        $crate::traits::graphable::create_place(&mut graph, ""),
                    );
                    if !self.tree.is_empty() {
                        $t::node_to_dot(&self, &mut graph, 0, &source, &sink);
                    }
                    Ok(graph)
                }
            }
        };
    }
    pub(crate) use tree_graphable;
    tree_graphable!(ProcessTree);

    impl ProcessTree {
        fn node_to_dot(
            &self,
            graph: &mut layout::topo::layout::VisualGraph,
            node: usize,
            entry: &Gateway,
            exit: &Gateway,
        ) -> usize {
            match self.tree[node] {
                TreeNode::Tau => {
                    graphable::create_edge(graph, entry.handle(), exit.handle(), "");
                    node + 1
                }
                TreeNode::Activity(activity) => {
                    let transition = graphable::create_transition(
                        graph,
                        self.activity_key.activity_to_label(&activity),
                        "",
                    );
                    graphable::create_edge(graph, entry.handle(), &transition, "");
                    graphable::create_edge(graph, &transition, exit.handle(), "");
                    node + 1
                }
                TreeNode::Operator(TreeOperator::Xor, number_of_children) => {
                    let mut child = node + 1;
                    let child_entry = if !entry.is_xor() {
                        let x = Gateway::Xor(graphable::create_dot(graph));
                        graphable::create_edge(graph, entry.handle(), x.handle(), "");
                        x
                    } else {
                        *entry
                    };
                    let child_exit = if !exit.is_xor() {
                        let x = Gateway::Xor(graphable::create_dot(graph));
                        graphable::create_edge(graph, x.handle(), exit.handle(), "");
                        x
                    } else {
                        *exit
                    };
                    for _ in 0..number_of_children {
                        child =
                            ProcessTree::node_to_dot(self, graph, child, &child_entry, &child_exit);
                    }
                    child
                }
                TreeNode::Operator(TreeOperator::Sequence, number_of_children) => {
                    let intermediate_nodes = (0..(number_of_children - 1))
                        .map(|_| Gateway::Xor(graphable::create_dot(graph)))
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

                        child =
                            ProcessTree::node_to_dot(self, graph, child, child_entry, child_exit);
                    }
                    child
                }
                TreeNode::Operator(TreeOperator::Concurrent, number_of_children) => {
                    let split = Gateway::And(graphable::create_gateway(graph, "+"));
                    graphable::create_edge(graph, entry.handle(), split.handle(), "");
                    let join = Gateway::And(graphable::create_gateway(graph, "+"));
                    graphable::create_edge(graph, join.handle(), exit.handle(), "");

                    let mut child = node + 1;
                    for _ in 0..number_of_children {
                        child = ProcessTree::node_to_dot(self, graph, child, &split, &join);
                    }
                    child
                }
                TreeNode::Operator(TreeOperator::Or, number_of_children) => {
                    let split = Gateway::Or(graphable::create_gateway(graph, "o"));
                    graphable::create_edge(graph, entry.handle(), split.handle(), "");
                    let join = Gateway::Or(graphable::create_gateway(graph, "o"));
                    graphable::create_edge(graph, join.handle(), exit.handle(), "");

                    let mut child = node + 1;
                    for _ in 0..number_of_children {
                        child = ProcessTree::node_to_dot(self, graph, child, &split, &join);
                    }
                    child
                }
                TreeNode::Operator(TreeOperator::Interleaved, number_of_children) => {
                    let split = Gateway::Int(graphable::create_gateway(graph, "↔"));
                    graphable::create_edge(graph, entry.handle(), split.handle(), "");
                    let join = Gateway::Int(graphable::create_gateway(graph, "↔"));
                    graphable::create_edge(graph, join.handle(), exit.handle(), "");

                    let mut child = node + 1;
                    for _ in 0..number_of_children {
                        child = ProcessTree::node_to_dot(self, graph, child, &split, &join);
                    }
                    child
                }
                TreeNode::Operator(TreeOperator::Loop, number_of_children) => {
                    let split = Gateway::Xor(graphable::create_dot(graph));
                    graphable::create_edge(graph, entry.handle(), split.handle(), "");
                    let join = Gateway::Xor(graphable::create_dot(graph));
                    graphable::create_edge(graph, join.handle(), exit.handle(), "");

                    let mut child = node + 1;

                    child = ProcessTree::node_to_dot(self, graph, child, &split, &join);

                    if number_of_children == 1 {
                        graphable::create_edge(graph, join.handle(), split.handle(), "");
                    } else {
                        for _ in 1..number_of_children {
                            child = ProcessTree::node_to_dot(self, graph, child, &join, &split);
                        }
                    }
                    child
                }
            }
        }
    }

    #[derive(EnumIs, Copy, Clone)]
    pub(crate) enum Gateway {
        Xor(NodeHandle),
        And(NodeHandle),
        Or(NodeHandle),
        Int(NodeHandle),
    }

    impl Gateway {
        pub(crate) fn handle(&self) -> &NodeHandle {
            match self {
                Gateway::Xor(h) | Gateway::And(h) | Gateway::Or(h) | Gateway::Int(h) => h,
            }
        }
    }
}

/// Input and output for process trees.
pub mod io {
    use strum::IntoEnumIterator;

    use crate::{
        ActivityKey, EbiObject, Exportable, Importable, ProcessTree, TreeMarking, TreeNode,
        TreeOperator,
        anyhow::{Context, Error, Result, anyhow},
        ebi_objects::process_tree::HEADER,
        line_reader::LineReader,
        traits::importable::{ImporterParameter, ImporterParameterValues, from_string},
    };
    use std::{
        fmt::{Display, Formatter},
        str::FromStr,
    };

    impl Display for ProcessTree {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            writeln!(f, "{}", HEADER)?;
            if !self.tree.is_empty() {
                self.node_to_string(0, 0, f).map_err(|_| std::fmt::Error)?;
            } else {
                writeln!(f)?;
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
    A tree with the empty language is represented by a second empty line.
    
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
                .with_context(|| format!("Failed to read header, which should be {}.", HEADER))?;
            if head != HEADER {
                return Err(anyhow!(
                    "First line should be exactly `{}`, but found `{}` at line {}.",
                    HEADER,
                    lreader.get_last_line(),
                    lreader.get_last_line_number()
                ));
            }

            let mut activity_key = ActivityKey::default();
            let mut tree = vec![];
            Self::string_to_tree(&mut lreader, &mut tree, &mut activity_key, true)?;

            if lreader.next_line().is_ok() {
                return Err(anyhow!(
                    "The tree parsed correctly, however there are lines left in the file, from line {}.",
                    lreader.get_last_line_number()
                ));
            }

            Ok((activity_key, tree).into())
        }
    }
    from_string!(ProcessTree);

    impl Exportable for ProcessTree {
        fn export_from_object(object: EbiObject, f: &mut dyn std::io::Write) -> Result<()> {
            match object {
                EbiObject::ProcessTree(lpn) => lpn.export(f),
                EbiObject::StochasticProcessTree(slpn) => ProcessTree::from(slpn).export(f),
                _ => Err(anyhow!(
                    "Cannot export {} {} as a process tree.",
                    object.get_type().get_article(),
                    object.get_type()
                )),
            }
        }

        fn export(&self, f: &mut dyn std::io::Write) -> Result<()> {
            Ok(write!(f, "{}", self)?)
        }
    }

    impl ProcessTree {
        fn node_to_string(
            &self,
            indent: usize,
            node: usize,
            f: &mut std::fmt::Formatter<'_>,
        ) -> Result<usize> {
            let id = "\t".repeat(indent);
            match &self.tree[node] {
                TreeNode::Tau => {
                    writeln!(f, "{}tau", id)?;
                    Ok(node + 1)
                }
                TreeNode::Activity(activity) => {
                    writeln!(
                        f,
                        "{}activity {}",
                        id,
                        self.activity_key.activity_to_label(activity)
                    )?;
                    Ok(node + 1)
                }
                TreeNode::Operator(operator, number_of_children) => {
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
            tree: &mut Vec<TreeNode>,
            activity_key: &mut ActivityKey,
            root: bool,
        ) -> Result<()> {
            let node_type_line = lreader.next_line_string().with_context(|| {
                format!(
                    "Failed to read node {} at line {}.",
                    tree.len(),
                    lreader.get_last_line_number()
                )
            })?;

            if node_type_line.trim() == "tau" {
                tree.push(TreeNode::Tau);
            } else if node_type_line.trim_start().starts_with("activity ") {
                let label = node_type_line.trim_start()[9..].to_string();
                let activity = activity_key.process_activity(&label);
                tree.push(TreeNode::Activity(activity));
            } else if let Ok(operator) = node_type_line
                .trim_start()
                .trim_end()
                .parse::<TreeOperator>()
            {
                let number_of_children = lreader.next_line_index().with_context(|| {
                    format!(
                        "Failed to read number of children for node {} at line {}.",
                        tree.len(),
                        lreader.get_last_line_number()
                    )
                })?;
                if number_of_children < 1 {
                    return Err(anyhow!(
                        "The {operator} node with number {} at line {} has no children.",
                        tree.len(),
                        lreader.get_last_line_number()
                    ));
                }
                tree.push(TreeNode::Operator(operator, number_of_children));
                for _ in 0..number_of_children {
                    Self::string_to_tree(lreader, tree, activity_key, false)?;
                }
            } else if root && node_type_line.trim_start().is_empty() {
                //empty tree
                return Ok(());
            } else {
                return Err(anyhow!(
                    "Could not parse type of node {} at line {}. Expected `tau`, `activity <label>`, `concurrent`, `interleaved`, `loop`, `or`, `sequence` or `xor`.",
                    tree.len(),
                    lreader.get_last_line_number()
                ));
            }

            Ok(())
        }
    }

    impl FromStr for TreeOperator {
        type Err = Error;

        fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
            for op in TreeOperator::iter() {
                if s == op.to_string() {
                    return Ok(op);
                }
            }
            Err(anyhow!("Could not read `{s}` as an operator."))
        }
    }

    impl Display for TreeMarking {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            write!(f, "{:?}, terminated: {}", self.states, self.terminated)
        }
    }
}

/// Semantics belongs in the Ebi crate, not here in Ebi_objects,
/// however we use them to create a state space in several conversions.
/// As such, its bases functions are here.
pub mod semantics {
    use crate::{
        Activity, ProcessTree, TreeNode, TreeOperator,
        anyhow::{Result, anyhow},
    };
    use std::ops::{Index, IndexMut};
    use strum_macros::EnumIs;

    #[derive(
        Copy, Clone, strum_macros::Display, Debug, Eq, PartialEq, Hash, PartialOrd, Ord, EnumIs,
    )]
    pub enum NodeState {
        /// Indicates that the node can be executed (for a leaf), or that one of its children can be executed (for an operator)
        Enabled,
        /// Indicates that the node has started.
        Started,
        /// Used in POWL models, not in process trees
        NotYetExecuted,
        /// Indicates that a node is currently inactive and has completed or not started yet.
        Closed,
    }

    #[derive(Clone, Debug, Eq, PartialEq, Hash, PartialOrd, Ord)]
    pub struct TreeMarking {
        pub terminated: bool,
        pub states: Vec<NodeState>,
    }

    impl TreeMarking {
        pub fn new(size: usize) -> Self {
            Self {
                terminated: false,
                states: vec![NodeState::Closed; size],
            }
        }

        pub fn get(&self, index: usize) -> Option<&NodeState> {
            self.states.get(index)
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

    /// Defines semantics for process trees.
    /// To avoid confusion in the `Ebi` crate, these methods are not implemented on the struct, but merely add it to the file.
    macro_rules! tree_semantics {
    ($t:ident) => {
        pub fn get_initial_state(tree: &$t) -> Option<TreeMarking> {
            if tree.tree.is_empty() {
                None
            } else {
                let mut state = TreeMarking {
                    states: vec![NodeState::Closed; tree.len()],
                    terminated: false,
                };
                enable_node(tree, &mut state, tree.root());
                Some(state)
            }
        }

        /// Returns whether it is possible that this node now terminates (true), or that a leaf has to be executed first (false).
        pub(crate) fn can_terminate(tree: &$t, state: &TreeMarking, node: usize) -> bool {
            match tree[node] {
                TreeNode::Tau => state[node] == NodeState::Closed,
                TreeNode::Activity(_) => state[node] == NodeState::Closed,
                TreeNode::Operator(TreeOperator::Concurrent, _)
                | TreeNode::Operator(TreeOperator::Interleaved, _) => {
                    //these nodes can terminate if all of their children are either closed or can terminate
                    tree.children(node).all(|child| {
                        state[child] == NodeState::Closed || can_terminate(tree, state, child)
                    })
                }
                TreeNode::Operator(TreeOperator::Or, _) => {
                    //an or can terminate if at least one child can terminate, and the others can be withdrawn
                    let mut one_child_closed = false;
                    for child in tree.children(node) {
                        if can_terminate(tree, state, child) {
                            one_child_closed = true;
                        } else if !can_withdraw_enablement(tree, state, child) {
                            //if there is one child that is not closed and not withdrawn, we cannot terminate the or
                            return false;
                        }
                    }

                    one_child_closed
                }
                TreeNode::Operator(TreeOperator::Loop, number_of_children) => {
                    let body_child = tree.get_child(node, 0);
                    if state[node] == NodeState::Closed {
                        //if the loop is closed, it can terminate
                        return true;
                    }
                    if number_of_children == 1 {
                        // single-child loop: the loop may stop whenever the body may stop
                        return can_terminate(tree, state, body_child);
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
                TreeNode::Operator(TreeOperator::Sequence, number_of_children) => {
                    //a sequence node can terminate if all its non-last children are closed and the last child can terminate
                    for child_rank in 0..number_of_children - 1 {
                        if state[tree.get_child(node, child_rank)] != NodeState::Closed {
                            return false;
                        }
                    }
                    can_terminate(tree, state, tree.get_child(node, number_of_children - 1))
                }
                TreeNode::Operator(TreeOperator::Xor, _) => {
                    //an xor can terminate if all of its children are closed or can terminate
                    tree.children(node).all(|child| {
                        state[child] == NodeState::Closed || can_terminate(tree, state, child)
                    })
                }
            }
        }

        /// Starts a new iteration of a loop with a single child.
        fn restart_loop_body(tree: &$t, state: &mut TreeMarking, loop_node: usize) {
            let body = loop_node + 1;
            withdraw_enablement(tree, state, body);   // close the old iteration
            enable_node(tree, state, body);           // fresh iteration
            // the exit is no longer available until this iteration completes
            withdraw_enablement_next_sequential_nodes(tree, state, loop_node);
        }

        /// If `leaf` is closed but would be enabled in a fresh iteration of a
        /// single-child loop above it, returns that loop (innermost first).
        fn single_child_loop_for(tree: &$t, state: &TreeMarking, leaf: usize) -> Option<usize> {
            if state[leaf] != NodeState::Closed {
                return None;
            }
            for (ancestor, _) in tree.ancestors(leaf) {
                if matches!(tree.tree[ancestor], TreeNode::Operator(TreeOperator::Loop, 1))
                    && state[ancestor] == NodeState::Started
                    && can_terminate(tree, state, ancestor + 1)
                {
                    let mut restarted = state.clone();
                    restart_loop_body(tree, &mut restarted, ancestor);
                    if can_execute(tree, &restarted, leaf) {
                        return Some(ancestor);
                    }
                }
            }
            None
        }

        /// Returns whether it is possible to withdraw the enablement.
        fn can_withdraw_enablement(_tree: &$t, state: &TreeMarking, node: usize) -> bool {
            state[node] == NodeState::Enabled
        }

        pub(crate) fn can_execute(tree: &$t, state: &TreeMarking, node: usize) -> bool {
            match tree.get(node) {
                Some(TreeNode::Activity(_)) => {}
                Some(TreeNode::Tau) => {}
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
            for (parent, _) in tree.ancestors(node) {
                if let Some(TreeNode::Operator(TreeOperator::Interleaved, _)) =
                    tree.tree.get(parent)
                {
                    //count the number of started children
                    let started_children = tree.children(parent).fold(0, |count, child| {
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
                        unreachable!("Cannot have more than one child open in an interleaved node.")
                    }
                }

                previous_parent = parent;
            }

            true
        }

        fn enable_node(tree: &$t, state: &mut TreeMarking, node: usize) {
            state[node] = NodeState::Enabled;

            match tree.tree[node] {
                TreeNode::Tau => {}
                TreeNode::Activity(_) => {}
                TreeNode::Operator(TreeOperator::Concurrent, _)
                | TreeNode::Operator(TreeOperator::Interleaved, _)
                | TreeNode::Operator(TreeOperator::Or, _)
                | TreeNode::Operator(TreeOperator::Xor, _) => {
                    //enable all children
                    for child in tree.children(node) {
                        enable_node(tree, state, child);
                    }
                }
                TreeNode::Operator(TreeOperator::Sequence, _)
                | TreeNode::Operator(TreeOperator::Loop, _) => {
                    //enable the first child
                    enable_node(tree, state, tree.get_child(node, 0));
                }
            }
        }

        /// enable the nodes that are sequentially next from this node
        fn enable_next_sequential_nodes(tree: &$t, state: &mut TreeMarking, node: usize) {
            if let Some((parent, child_rank)) = tree.parent(node) {
                match tree.tree[parent] {
                    TreeNode::Tau | TreeNode::Activity(_) => {
                        unreachable!("Taus and activities cannot have children.")
                    }
                    TreeNode::Operator(TreeOperator::Xor, _) => {
                        //propagate to parent
                        enable_next_sequential_nodes(tree, state, parent);
                    }
                    TreeNode::Operator(TreeOperator::Concurrent, _)
                    | TreeNode::Operator(TreeOperator::Or, _)
                    | TreeNode::Operator(TreeOperator::Interleaved, _) => {
                        if can_terminate(tree, state, parent) {
                            //this parent can terminate, so we propagate to parent
                            enable_next_sequential_nodes(tree, state, parent);
                        } else {
                            //this parent cannot yet terminate, so there is nothing to enable
                        }
                    }
                    TreeNode::Operator(TreeOperator::Loop, number_of_children) => {
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
                    TreeNode::Operator(TreeOperator::Sequence, number_of_children) => {
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
            // println!("\t\twithdraw enablement next sequential nodes node {node}");
            if let Some((parent, child_rank)) = tree.parent(node) {
                match tree.tree[parent] {
                    TreeNode::Tau | TreeNode::Activity(_) => {
                        unreachable!("Taus and activities cannot have children.")
                    }
                    TreeNode::Operator(TreeOperator::Sequence, number_of_children) => {
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
                    TreeNode::Operator(TreeOperator::Concurrent, _)
                    | TreeNode::Operator(TreeOperator::Or, _)
                    | TreeNode::Operator(TreeOperator::Interleaved, _)
                    | TreeNode::Operator(TreeOperator::Xor, _) => {
                        //propagate to parent
                        withdraw_enablement_next_sequential_nodes(tree, state, parent);
                    }
                    TreeNode::Operator(TreeOperator::Loop, number_of_children) => {
                        if child_rank == 0 {
                            //we were the body child, withdraw redo children
                            for child_rank in 1..number_of_children {
                                withdraw_enablement(
                                    tree,
                                    state,
                                    tree.get_child(parent, child_rank),
                                );
                            }
                            //propagate to parent
                            withdraw_enablement_next_sequential_nodes(tree, state, parent);
                        } else {
                            //we were a redo child, withdraw the body child
                            withdraw_enablement(tree, state, tree.get_child(parent, 0));
                        }
                    }
                }
            }
        }

        fn close_node(tree: &$t, state: &mut TreeMarking, node: usize) {
            // println!("\tclose node {node} state {state}");

            //close this node and all of its children
            for grandchild in node..tree.traverse(node) {
                state[grandchild] = NodeState::Closed;
            }

            //this may open another node, based on the operator of the parent
            if let Some((parent, child_rank)) = tree.parent(node) {
                match tree.tree[parent] {
                    TreeNode::Tau | TreeNode::Activity(_) => {
                        unreachable!("Taus and activities cannot have children.")
                    }
                    TreeNode::Operator(TreeOperator::Sequence, number_of_children) => {
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
                    TreeNode::Operator(TreeOperator::Concurrent, _)
                    | TreeNode::Operator(TreeOperator::Interleaved, _) => {
                        //for a concurrent parent, the parent can be closed if all of its children have been closed
                        if tree
                            .children(parent)
                            .all(|child| state[child] == NodeState::Closed)
                        {
                            //close the parent
                            close_node(tree, state, parent);
                        }

                        if can_terminate(tree, state, parent) {
                            enable_next_sequential_nodes(tree, state, parent);
                        }
                    }
                    TreeNode::Operator(TreeOperator::Or, _) => {
                        //for an or parent, the parent can be closed if all of its children have been closed
                        if tree
                            .children(parent)
                            .all(|child| state[child] == NodeState::Closed)
                        {
                            //close the parent
                            close_node(tree, state, parent);
                        }

                        //furthermore, we may continue with the next sequential sibling of the parent
                        if can_terminate(tree, state, parent) {
                            enable_next_sequential_nodes(tree, state, parent);
                        }
                    }
                    TreeNode::Operator(TreeOperator::Xor, _) => {
                        //for a xor parent, the parent can be closed as we executed one of its children
                        close_node(tree, state, parent);
                    }
                    TreeNode::Operator(TreeOperator::Loop, number_of_children) => {
                        //for a loop parent, we open the next child(ren)
                        if child_rank == 0 {
                            //enable the non-first/redo siblings
                            for child_rank in 1..number_of_children {
                                enable_node(tree, state, tree.get_child(parent, child_rank));
                            }
                            //enable the exit
                            enable_next_sequential_nodes(tree, state, parent);
                        } else {
                            //enable the first/body child
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
            // println!("\tstart node {node}, child {:?} state {state}", child);
            match tree.tree[node] {
                TreeNode::Tau
                | TreeNode::Activity(_)
                | TreeNode::Operator(TreeOperator::Concurrent, _) => {
                    if state[node] != NodeState::Started {
                        state[node] = NodeState::Started;

                        //recurse to parent
                        if let Some((parent, _)) = tree.parent(node) {
                            start_node(tree, state, parent, Some(node));
                        }
                    }
                }
                TreeNode::Operator(TreeOperator::Sequence, _) => {
                    //for a sequence parent, we withdraw the enablement of the previous child
                    if let Some(child) = child
                        && let Some(child_rank) = tree.get_child_rank_with(node, child)
                        && child_rank > 0
                    {
                        let previous_child = tree.get_child(node, child_rank - 1);
                        withdraw_enablement(tree, state, previous_child);
                    }

                    if state[node] != NodeState::Started {
                        state[node] = NodeState::Started;
                        //recurse to parent
                        if let Some((parent, _)) = tree.parent(node) {
                            start_node(tree, state, parent, Some(node));
                        }
                    }
                }
                TreeNode::Operator(TreeOperator::Interleaved, _) => {
                    if state[node] != NodeState::Started {
                        state[node] = NodeState::Started;

                        //recurse to parent
                        if let Some((parent, _)) = tree.parent(node) {
                            start_node(tree, state, parent, Some(node));
                        }
                    } else if let Some(from_child) = child {
                        //we are starting a non-first child of an interleaved node: withdraw enablement in the finished grand child

                        //find the previous child that was started
                        for sibling in tree.children(node) {
                            if sibling != from_child && state[sibling] == NodeState::Started {
                                close_node(tree, state, sibling);
                            }
                        }
                    } else {
                        unreachable!(
                            "An interleaved node must either start anew or be started from a child."
                        )
                    }
                }
                TreeNode::Operator(TreeOperator::Loop, number_of_children) => {
                    if state[node] != NodeState::Started {
                        state[node] = NodeState::Started;

                        //recurse to parent
                        if let Some((parent, _)) = tree.parent(node) {
                            start_node(tree, state, parent, Some(node));
                        }
                    } else {
                        //for a loop, even though it started before, we may need to withdraw enablement of next sequential nodes

                        if let Some(child) = child
                            && child > node + 1
                        {
                            //we are in a redo child: withdraw the next sequential node, which cannot fire now
                            withdraw_enablement_next_sequential_nodes(tree, state, node);
                            //also withdraw enablement of all other children
                            for child_rank in 0..number_of_children {
                                let sibling = tree.get_child(node, child_rank);
                                if sibling != child {
                                    withdraw_enablement(tree, state, sibling);
                                }
                            }
                        } else {
                            //we are in a body child; withdraw the enablement of the redo children
                            for child_rank in 1..number_of_children {
                                withdraw_enablement(tree, state, tree.get_child(node, child_rank));
                            }
                        }

                        //recurse to parent
                        if let Some((parent, _)) = tree.parent(node) {
                            start_node(tree, state, parent, Some(node));
                        }
                    }
                }
                TreeNode::Operator(TreeOperator::Xor, _) => {
                    if state[node] != NodeState::Started {
                        state[node] = NodeState::Started;

                        //for an xor, the siblings of the child must be withdrawn
                        for child2 in tree.children(node) {
                            if let Some(child) = child {
                                if child2 != child {
                                    withdraw_enablement(tree, state, child2);
                                }
                            }
                        }
                        //recurse to parent
                        if let Some((parent, _)) = tree.parent(node) {
                            start_node(tree, state, parent, Some(node));
                        }
                    }
                }
                TreeNode::Operator(TreeOperator::Or, _) => {
                    if state[node] != NodeState::Started {
                        state[node] = NodeState::Started;
                        if let Some((parent, _)) = tree.parent(node) {
                            start_node(tree, state, parent, Some(node));
                        }
                    } else if !can_terminate(tree, state, node) {
                        // a child just committed, so successors enabled earlier
                        // (because the or could finish) must be withdrawn
                        withdraw_enablement_next_sequential_nodes(tree, state, node);
                    }
                }
            }
        }

        pub fn get_number_of_transitions(tree: &$t) -> usize {
            tree.tree.iter().filter(|node| node.is_leaf()).count() + 1 //the last transition is explicit termination, which is required by the semantics of Ebi
        }

        pub fn get_enabled_transitions(tree: &$t, state: &TreeMarking) -> Vec<$crate::TransitionIndex> {
            let mut result = vec![];

            for (transition_index, node) in tree.leaves().iter().enumerate() {
                if can_execute(tree, state, *node) || single_child_loop_for(tree, state, *node).is_some() {
                    result.push(transition_index);
                }
            }

            if !state.terminated && can_terminate(tree, state, tree.root()) {
                result.push(tree.number_of_leaves());
            }

            result
        }

        pub fn is_final_state(_tree: &$t, state: &TreeMarking) -> bool {
            state.terminated
        }

        pub fn get_transition_activity(tree: &$t, transition: $crate::TransitionIndex) -> Option<Activity> {
            let node = tree.transition_to_node(transition)?;
            match tree.tree[node] {
                TreeNode::Tau => None,
                TreeNode::Activity(activity) => Some(activity),
                TreeNode::Operator(_, _) => None,
            }
        }

        /// Verifies that the transition is enabled and updates the state to execute it.
        ///
        /// If the caller can guarantee that the transition is enabled, then `execute_transition_unchecked` is a faster alternative.
        pub fn execute_transition(
            tree: &$t,
            state: &mut TreeMarking,
            transition: $crate::TransitionIndex,
        ) -> Result<()> {
            if !get_enabled_transitions(tree, state).contains(&transition) {
                return Err(anyhow!("Transition {transition} is not enabled."));
            }

            execute_transition_unchecked(tree, state, transition);
            Ok(())
        }

        /// Execute the given transition.
        ///
        /// For performance reasons, this method does not check whether the transition is actually enabled.
        /// It is the responsibility of the caller to ensure the transition is enabled.
        /// If a non-enabled transition is executed, the resulting state is undefined.
        pub fn execute_transition_unchecked(
            tree: &$t,
            state: &mut TreeMarking,
            transition: $crate::TransitionIndex,
        ) {
            if transition >= tree.number_of_leaves() {
                state.terminated = true;
                state.states.fill(NodeState::Closed);
            } else {
                let node = tree.transition_to_node(transition).unwrap();
                if let Some(loop_node) = single_child_loop_for(tree, state, node) {
                    restart_loop_body(tree, state, loop_node);
                }
                start_node(tree, state, node, None);
                close_node(tree, state, node);
            }
            // println!("state after executing transition {transition}: {}", state);
        }
    };
}
    pub(crate) use tree_semantics;
    tree_semantics!(ProcessTree);
}

#[cfg(any(test, feature = "testactivities"))]
impl TestActivityKey for ProcessTree {
    fn test_activity_key(&self) {
        self.tree.iter().for_each(|node| {
            if let TreeNode::Activity(a) = node {
                self.activity_key().assert_activity_is_of_key(a);
            }
        });
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        HasActivityKey, ProcessTree, StochasticProcessTree, activity, con,
        ebi_objects::{
            process_tree::{
                TreeNode,
                semantics::{
                    execute_transition, get_enabled_transitions, get_initial_state,
                    get_transition_activity,
                },
            },
            scalable_vector_graphics::ToSVG,
        },
        or, seq, tau, tloop, xor,
    };
    use std::fs;

    #[test]
    fn tree_create() {
        let xor = xor!(tau!(), activity!("a"));

        assert_eq!(xor.len(), 3)
    }

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

    #[test]
    fn ptree_semantics_interleaved_3() {
        let fin1 = fs::read_to_string("testfiles/interleaved.ptree").unwrap();
        let tree: ProcessTree = fin1.parse::<ProcessTree>().unwrap();

        let ta = 0;
        let tb = 1;
        let tc = 2;
        let tt = 3;

        let mut state = get_initial_state(&tree).unwrap();
        println!("{}\n", state);
        assert_eq!(get_enabled_transitions(&tree, &state), [ta, tb, tc]);

        println!("execute a {}", ta);
        execute_transition(&tree, &mut state, ta).unwrap();
        println!("{}\n", state);
        assert_eq!(get_enabled_transitions(&tree, &state), [tb, tc]);

        println!("execute b {}", tb);
        execute_transition(&tree, &mut state, tb).unwrap();
        println!("{}\n", state);
        assert_eq!(get_enabled_transitions(&tree, &state), [tc]);

        println!("execute c {}", tc);
        execute_transition(&tree, &mut state, tc).unwrap();
        println!("{}\n", state);
        assert_eq!(get_enabled_transitions(&tree, &state), [tt]);
    }

    #[test]
    fn ptree_semantics_interleaved_4() {
        let fin1 =
            fs::read_to_string("testfiles/loop(interleaved(seq(a,b),loop(c,d),e).sptree").unwrap();
        let tree: ProcessTree = fin1.parse::<StochasticProcessTree>().unwrap().into();

        let ta = 0;
        let tb = 1;
        let tc = 2;
        let td = 3;
        let te = 4;
        let tt = 5;

        let mut state = get_initial_state(&tree).unwrap();
        println!("{}\n", state);
        assert_eq!(get_enabled_transitions(&tree, &state), [ta, tc]);

        println!("execute a {}", ta);
        execute_transition(&tree, &mut state, ta).unwrap();
        println!("{}\n", state);
        assert_eq!(get_enabled_transitions(&tree, &state), [tb]);

        println!("execute b {}", tb);
        execute_transition(&tree, &mut state, tb).unwrap();
        println!("{}\n", state);
        assert_eq!(get_enabled_transitions(&tree, &state), [tc]);

        println!("execute c {}", tc);
        execute_transition(&tree, &mut state, tc).unwrap();
        println!("{}\n", state);
        assert_eq!(get_enabled_transitions(&tree, &state), [td, te, tt]);

        println!("execute d {}", td);
        execute_transition(&tree, &mut state, td).unwrap();
        println!("{}\n", state);
        assert_eq!(get_enabled_transitions(&tree, &state), [tc]);

        println!("execute c {}", tc);
        execute_transition(&tree, &mut state, tc).unwrap();
        println!("{}\n", state);
        assert_eq!(get_enabled_transitions(&tree, &state), [td, te, tt]);

        println!("execute e {}", te);
        execute_transition(&tree, &mut state, te).unwrap();
        println!("{}\n", state);
        assert_eq!(get_enabled_transitions(&tree, &state), [ta, tc]);

        println!("execute c {}", tc);
        execute_transition(&tree, &mut state, tc).unwrap();
        println!("{}\n", state);
        assert_eq!(get_enabled_transitions(&tree, &state), [ta, td]);

        println!("execute a {}", ta);
        execute_transition(&tree, &mut state, ta).unwrap();
        println!("{}\n", state);
        assert_eq!(get_enabled_transitions(&tree, &state), [tb]);

        println!("execute b {}", tb);
        execute_transition(&tree, &mut state, tb).unwrap();
        println!("{}\n", state);
        assert_eq!(get_enabled_transitions(&tree, &state), [te, tt]);

        println!("execute tt {}", tt);
        execute_transition(&tree, &mut state, tt).unwrap();
        println!("{}\n", state);
        assert_eq!(get_enabled_transitions(&tree, &state), [0; 0]);
    }

    // #[test]
    #[allow(dead_code)]
    fn jan_tree_graph() {
        let fin1 = fs::read_to_string(
            "testfiles/IMPF_BPI_Challenge_2020_International_Declarations_178181.xes.ptree",
        )
        .unwrap();
        let tree = fin1.parse::<ProcessTree>().unwrap();

        let svg = tree.to_svg().unwrap();

        fs::write(
            "testfiles/IMPF_BPI_Challenge_2020_International_Declarations_178181.xes.ptree.svg",
            svg.to_string(),
        )
        .unwrap();

        println!("{svg}");
    }

    #[test]
    #[allow(nonstandard_style)]
    fn jan_tree() {
        let fin1 = fs::read_to_string(
            "testfiles/IMPF_BPI_Challenge_2020_International_Declarations_178181.xes.ptree",
        )
        .unwrap();
        let tree = fin1.parse::<ProcessTree>().unwrap();

        // println!("{:?}", tree.activity_key());

        let activity_End_trip = 0;
        let activity_start_trip = 1;
        let Declaration_REJECTED_by_EMPLOYEE = 2;
        let Send_Reminder = 4;
        let Declaration_SUBMITTED_by_EMPLOYEE = 6;
        let tau_21_line_49 = 8;
        let tau_25_line_59 = 9;
        let Permit_REJECTED_by_MISSING = 10;
        let tau_29_line_67 = 11;
        let Permit_APPROVED_by_SUPERVISOR_line_71 = 12;
        let _tau_32_line_72 = 13;
        let tau_36_line_82 = 14;
        let Permit_APPROVED_by_PRE_APPROVER_line_86 = 15;
        let _tau_39_line_87 = 16;
        let Permit_SUBMITTED_by_EMPLOYEE_line_97 = 17;
        let Permit_REJECTED_by_PRE_APPROVER_line_101 = 18;
        let Permit_REJECTED_by_ADMINISTRATION_line_102 = 19;
        let tau_line_109 = 20;
        let Permit_APPROVED_by_ADMINISTRATION_line_110 = 21;
        let Permit_APPROVED_by_BUDGET_OWNER_line_121 = 23;
        let tau_line_125 = 24;
        let tau_line_132 = 28;
        let tau_line_138 = 31;
        let Permit_FINAL_APPROVED_by_DIRECTOR_line_144 = 34;

        let n = Permit_FINAL_APPROVED_by_DIRECTOR_line_144;
        match tree[tree.transition_to_node(n).unwrap()] {
            TreeNode::Activity(x) => println!("--{}", tree.activity_key.activity_to_label(&x)),
            TreeNode::Tau => println!("-tau {:?}", tree.transition_to_node(n)),
            _ => {}
        }

        let mut state = get_initial_state(&tree).unwrap();
        assert_eq!(
            get_enabled_transitions(&tree, &state),
            [
                activity_End_trip,
                activity_start_trip,
                Declaration_REJECTED_by_EMPLOYEE,
                Send_Reminder,
                Declaration_SUBMITTED_by_EMPLOYEE,
                tau_21_line_49,
                tau_25_line_59,
                Permit_REJECTED_by_MISSING,
            ]
        );

        println!("execute tau - 9");
        execute_transition(&tree, &mut state, tau_25_line_59).unwrap();
        assert_eq!(
            get_enabled_transitions(&tree, &state),
            [
                activity_End_trip,
                activity_start_trip,
                Declaration_REJECTED_by_EMPLOYEE,
                Send_Reminder,
                Declaration_SUBMITTED_by_EMPLOYEE,
                tau_29_line_67,
                Permit_APPROVED_by_SUPERVISOR_line_71,
                tau_36_line_82,
                Permit_APPROVED_by_PRE_APPROVER_line_86,
                Permit_SUBMITTED_by_EMPLOYEE_line_97,
            ]
        );

        println!("execute Permit SUBMITTED by EMPLOYEE - 17");
        execute_transition(&tree, &mut state, Permit_SUBMITTED_by_EMPLOYEE_line_97).unwrap();
        assert_eq!(
            get_enabled_transitions(&tree, &state),
            [
                activity_End_trip,
                activity_start_trip,
                Declaration_REJECTED_by_EMPLOYEE,
                Send_Reminder,
                Declaration_SUBMITTED_by_EMPLOYEE,
                tau_29_line_67,
                Permit_APPROVED_by_SUPERVISOR_line_71,
                tau_36_line_82,
                Permit_APPROVED_by_PRE_APPROVER_line_86,
                Permit_REJECTED_by_PRE_APPROVER_line_101,
                Permit_REJECTED_by_ADMINISTRATION_line_102,
                tau_line_109,
                Permit_APPROVED_by_ADMINISTRATION_line_110
            ]
        );

        println!("execute Permit APPROVED by ADMINISTRATION - 21");
        assert!(
            get_enabled_transitions(&tree, &state)
                .contains(&Permit_APPROVED_by_ADMINISTRATION_line_110)
        );
        execute_transition(
            &tree,
            &mut state,
            Permit_APPROVED_by_ADMINISTRATION_line_110,
        )
        .unwrap();

        println!("execute Permit APPROVED by BUDGET OWNER - 23");
        assert!(
            get_enabled_transitions(&tree, &state)
                .contains(&Permit_APPROVED_by_BUDGET_OWNER_line_121)
        );
        execute_transition(&tree, &mut state, Permit_APPROVED_by_BUDGET_OWNER_line_121).unwrap();

        println!("execute tau - 24");
        assert!(get_enabled_transitions(&tree, &state).contains(&tau_line_125));
        execute_transition(&tree, &mut state, tau_line_125).unwrap();

        println!("execute tau - 28");
        assert!(get_enabled_transitions(&tree, &state).contains(&tau_line_132));
        execute_transition(&tree, &mut state, tau_line_132).unwrap();

        println!("execute tau - 14");
        assert!(get_enabled_transitions(&tree, &state).contains(&tau_36_line_82));
        execute_transition(&tree, &mut state, tau_36_line_82).unwrap();

        println!("execute tau - 31");
        assert!(get_enabled_transitions(&tree, &state).contains(&tau_line_138));
        execute_transition(&tree, &mut state, tau_line_138).unwrap();

        println!("execute Permit APPROVED by SUPERVISOR - 12");
        assert!(
            get_enabled_transitions(&tree, &state).contains(&Permit_APPROVED_by_SUPERVISOR_line_71)
        );
        execute_transition(&tree, &mut state, Permit_APPROVED_by_SUPERVISOR_line_71).unwrap();

        println!("execute Permit_FINAL_APPROVED_by_DIRECTOR - 34");
        assert!(
            get_enabled_transitions(&tree, &state)
                .contains(&Permit_FINAL_APPROVED_by_DIRECTOR_line_144)
        );
        execute_transition(
            &tree,
            &mut state,
            Permit_FINAL_APPROVED_by_DIRECTOR_line_144,
        )
        .unwrap();
    }

    #[test]
    fn seq_or_a_b_c_d() {
        let fin1 = fs::read_to_string("testfiles/seq(or(a,b),c,d).ptree").unwrap();
        let tree = fin1.parse::<ProcessTree>().unwrap();

        const TA: usize = 0;
        const TB: usize = 1;
        const TC: usize = 2;
        const TD: usize = 3;
        const TFIN: usize = 4;

        let mut state = get_initial_state(&tree).unwrap();
        assert_eq!(get_enabled_transitions(&tree, &state), [TA, TB]);

        execute_transition(&tree, &mut state, TA).unwrap();
        assert_eq!(get_enabled_transitions(&tree, &state), [TB, TC]);

        execute_transition(&tree, &mut state, TC).unwrap();
        assert_eq!(get_enabled_transitions(&tree, &state), [TD]);

        execute_transition(&tree, &mut state, TD).unwrap();
        assert_eq!(get_enabled_transitions(&tree, &state), [TFIN]);

        execute_transition(&tree, &mut state, TFIN).unwrap();
        assert!(get_enabled_transitions(&tree, &state).is_empty());
    }

    #[test]
    fn seq_or_a_seq_b1_b2_c() {
        let fin1 = fs::read_to_string("testfiles/seq(or(a,seq(b1,b2)),c).ptree").unwrap();
        let tree = fin1.parse::<ProcessTree>().unwrap();

        const A: usize = 0;
        const B1: usize = 1;
        const B2: usize = 2;
        const C: usize = 3;
        const FIN: usize = 4;

        let mut state = get_initial_state(&tree).unwrap();
        assert_eq!(get_enabled_transitions(&tree, &state), [A, B1]);

        execute_transition(&tree, &mut state, B1).unwrap();
        assert_eq!(get_enabled_transitions(&tree, &state), [A, B2]);

        execute_transition(&tree, &mut state, A).unwrap();
        assert_eq!(get_enabled_transitions(&tree, &state), [B2]);

        execute_transition(&tree, &mut state, B2).unwrap();
        assert_eq!(get_enabled_transitions(&tree, &state), [C]);

        execute_transition(&tree, &mut state, C).unwrap();
        assert_eq!(get_enabled_transitions(&tree, &state), [FIN]);
    }

    #[test]
    fn or_skip_sibling_branch() {
        let fin1 = fs::read_to_string("testfiles/seq(or(a,seq(b1,b2)),c).ptree").unwrap();
        let tree = fin1.parse::<ProcessTree>().unwrap();

        const A: usize = 0;
        const B1: usize = 1;
        const _B2: usize = 2;
        const C: usize = 3;
        const FIN: usize = 4;

        let mut state = get_initial_state(&tree).unwrap();
        assert_eq!(get_enabled_transitions(&tree, &state), [A, B1]);

        execute_transition(&tree, &mut state, A).unwrap();
        execute_transition(&tree, &mut state, C).unwrap();
        assert_eq!(get_enabled_transitions(&tree, &state), [FIN]);
    }

    #[test]
    fn or_successor_withdrawn_when_sibling_starts() {
        let fin1 = fs::read_to_string("testfiles/seq(or(a,seq(b1,b2)),c).ptree").unwrap();
        let tree = fin1.parse::<ProcessTree>().unwrap();

        const A: usize = 0;
        const B1: usize = 1;
        const B2: usize = 2;
        const C: usize = 3;

        let mut state = get_initial_state(&tree).unwrap();

        execute_transition(&tree, &mut state, A).unwrap();
        assert_eq!(get_enabled_transitions(&tree, &state), [B1, C]);

        execute_transition(&tree, &mut state, B1).unwrap();
        assert_eq!(get_enabled_transitions(&tree, &state), [B2]);

        execute_transition(&tree, &mut state, B2).unwrap();
        assert_eq!(get_enabled_transitions(&tree, &state), [C]);
    }

    #[test]
    fn parse_invalid() {
        let fin1 = fs::read_to_string("testfiles/invalid-tree.ptree").unwrap();
        _ = fin1.parse::<ProcessTree>().unwrap_err();
    }

    #[test]
    fn remove_node() {
        let mut tree1 = xor!(tau!(), activity!("a"), activity!("b"));
        let tree2 = xor!(tau!(), activity!("b"));
        let tree3 = xor!(tau!());
        let tree4 = ProcessTree::default();

        tree1.remove(2).unwrap();

        assert_eq!(tree1, tree2);

        tree1.remove(2).unwrap();

        assert_eq!(tree1, tree3);

        tree1.remove(1).unwrap();

        assert_eq!(tree1, tree4);

        tree1.remove(0).unwrap_err();
    }

    #[test]
    fn sequence_concurrent_loop_a_tau_b_c() {
        let tree = seq!(
            con!(tloop!(activity!("a"), tau!()), activity!("b")),
            activity!("c")
        );

        const A: usize = 0;
        const TAU: usize = 1;
        const B: usize = 2;
        const C: usize = 3;
        const FIN: usize = 4;

        let mut state = get_initial_state(&tree).unwrap();
        assert_eq!(get_enabled_transitions(&tree, &state), [A, B]);

        execute_transition(&tree, &mut state, A).unwrap();
        assert_eq!(get_enabled_transitions(&tree, &state), [TAU, B]);

        // b finishes last: the concurrent node can now terminate
        execute_transition(&tree, &mut state, B).unwrap();
        assert_eq!(get_enabled_transitions(&tree, &state), [TAU, C]);

        execute_transition(&tree, &mut state, C).unwrap();
        assert_eq!(get_enabled_transitions(&tree, &state), [FIN]);

        execute_transition(&tree, &mut state, FIN).unwrap();
        assert!(get_enabled_transitions(&tree, &state).is_empty());
    }

    #[test]
    fn concurrent_loop_exit_enabled_when_loop_finishes_last() {
        let tree = seq!(
            con!(tloop!(activity!("a"), tau!()), activity!("b")),
            activity!("c")
        );

        const A: usize = 0;
        const TAU: usize = 1;
        const B: usize = 2;
        const C: usize = 3;
        const FIN: usize = 4;

        let mut state = get_initial_state(&tree).unwrap();
        assert_eq!(get_enabled_transitions(&tree, &state), [A, B]);

        execute_transition(&tree, &mut state, B).unwrap();
        assert_eq!(get_enabled_transitions(&tree, &state), [A]);

        execute_transition(&tree, &mut state, A).unwrap();
        assert_eq!(get_enabled_transitions(&tree, &state), [TAU, C]);

        execute_transition(&tree, &mut state, C).unwrap();
        assert_eq!(get_enabled_transitions(&tree, &state), [FIN]);
    }

    #[test]
    fn remove_recursive_leaf_with_siblings() {
        let mut tree = xor!(tau!(), activity!("a"), activity!("b"));
        // nodes: 0 xor(3), 1 tau, 2 a, 3 b

        let removed = tree.remove_recursive(2).unwrap();

        assert_eq!(removed.len(), 1);
        assert_eq!(tree.len(), 3);
        assert_eq!(tree, xor!(tau!(), activity!("b")));
    }

    #[test]
    fn remove_recursive_subtree_with_siblings() {
        let mut tree = con!(seq!(activity!("a"), activity!("b")), activity!("c"));
        // nodes: 0 con(2), 1 seq(2), 2 a, 3 b, 4 c

        let removed = tree.remove_recursive(1).unwrap();

        assert_eq!(removed.len(), 3); // seq, a, b
        assert_eq!(tree.len(), 2);
        assert_eq!(tree[0].number_of_children(), 1);
        assert_eq!(tree, con!(activity!("c")));
    }

    #[test]
    fn remove_recursive_single_child_subtree_with_siblings() {
        let mut tree = xor!(seq!(activity!("a")), activity!("b"));
        // nodes: 0 xor(2), 1 seq(1), 2 a, 3 b

        tree.remove_recursive(1).unwrap();

        assert_eq!(tree.len(), 2);
        assert_eq!(tree[0].number_of_children(), 1);
        assert_eq!(tree, xor!(activity!("b")));
    }

    #[test]
    fn remove_recursive_only_child_removes_empty_parent() {
        let mut tree = seq!(activity!("a"), xor!(activity!("b")), activity!("c"));
        // nodes: 0 seq(3), 1 a, 2 xor(1), 3 b, 4 c

        tree.remove_recursive(3).unwrap();

        assert_eq!(tree.len(), 3);
        assert_eq!(tree[0].number_of_children(), 2);
        assert_eq!(tree, seq!(activity!("a"), activity!("c")));
    }

    #[test]
    fn remove_recursive_out_of_range() {
        let mut tree = xor!(tau!(), activity!("a"));
        tree.remove_recursive(tree.len()).unwrap_err();
        tree.remove_recursive(100).unwrap_err();
        assert_eq!(tree.len(), 3); // untouched
    }

    #[test]
    fn empty_tree() {
        let fin = "process tree";
        assert!(fin.parse::<ProcessTree>().is_err());

        let fin = "process tree\n";
        assert!(fin.parse::<ProcessTree>().is_err());

        let fin = "process tree\n\n";
        assert_eq!(fin.parse::<ProcessTree>().unwrap(), ProcessTree::default());

        assert_eq!(
            ProcessTree::default(),
            ProcessTree::default()
                .to_string()
                .parse::<ProcessTree>()
                .unwrap()
        );
    }

    #[test]
    fn remove() {
        let mut tree = seq!(activity!("a"), xor!(activity!("b")));
        tree.remove(3).unwrap();
        assert_eq!(tree, seq!(activity!("a")));
    }

    #[test]
    fn tau_loop() {
        let fin = "process tree\nloop\n1\nactivity a";
        let tree = fin.parse::<ProcessTree>().unwrap();

        const A: usize = 0;
        const FIN: usize = 1;

        let mut state = get_initial_state(&tree).unwrap();
        assert_eq!(get_enabled_transitions(&tree, &state), [A]);

        execute_transition(&tree, &mut state, A).unwrap();
        assert_eq!(get_enabled_transitions(&tree, &state), [A, FIN]);

        execute_transition(&tree, &mut state, A).unwrap();
        assert_eq!(get_enabled_transitions(&tree, &state), [A, FIN]);

        execute_transition(&tree, &mut state, A).unwrap();
        assert_eq!(get_enabled_transitions(&tree, &state), [A, FIN]);

        execute_transition(&tree, &mut state, FIN).unwrap();
        assert_eq!(get_enabled_transitions(&tree, &state), vec![0; 0]);
    }

    #[test]
    fn loop_seq() {
        let tree = tloop!(seq!(activity!("a"), activity!("b")));

        const A: usize = 0;
        const B: usize = 1;
        const FIN: usize = 2;

        let mut state = get_initial_state(&tree).unwrap();
        assert_eq!(get_enabled_transitions(&tree, &state), [A]);

        execute_transition(&tree, &mut state, A).unwrap();
        assert_eq!(get_enabled_transitions(&tree, &state), [B]);

        execute_transition(&tree, &mut state, B).unwrap();
        assert_eq!(get_enabled_transitions(&tree, &state), [A, FIN]);

        execute_transition(&tree, &mut state, FIN).unwrap();
        assert_eq!(get_enabled_transitions(&tree, &state), vec![0; 0]);
    }

    #[test]
    fn loop_or() {
        let tree = tloop!(or!(activity!("a"), activity!("b")));

        const A: usize = 0;
        const B: usize = 1;
        const FIN: usize = 2;

        let mut state = get_initial_state(&tree).unwrap();
        assert_eq!(get_enabled_transitions(&tree, &state), [A, B]);

        execute_transition(&tree, &mut state, A).unwrap();
        assert_eq!(get_enabled_transitions(&tree, &state), [A, B, FIN]);

        execute_transition(&tree, &mut state, A).unwrap();
        assert_eq!(get_enabled_transitions(&tree, &state), [A, B, FIN]);

        execute_transition(&tree, &mut state, B).unwrap();
        assert_eq!(get_enabled_transitions(&tree, &state), [A, B, FIN]);

        execute_transition(&tree, &mut state, FIN).unwrap();
        assert_eq!(get_enabled_transitions(&tree, &state), [0; 0]);
    }

    #[test]
    fn loop_or_seq() {
        let tree = tloop!(or!(activity!("a"), seq!(activity!("b"), activity!("c"))));

        const A: usize = 0;
        const B: usize = 1;
        const C: usize = 2;
        const FIN: usize = 3;

        let mut state = get_initial_state(&tree).unwrap();
        assert_eq!(get_enabled_transitions(&tree, &state), [A, B]);

        execute_transition(&tree, &mut state, A).unwrap();
        assert_eq!(get_enabled_transitions(&tree, &state), [A, B, FIN]);

        execute_transition(&tree, &mut state, B).unwrap();
        assert_eq!(get_enabled_transitions(&tree, &state), [C]);

        execute_transition(&tree, &mut state, C).unwrap();
        assert_eq!(get_enabled_transitions(&tree, &state), [A, B, FIN]);

        execute_transition(&tree, &mut state, FIN).unwrap();
        assert_eq!(get_enabled_transitions(&tree, &state), [0; 0]);
    }

    #[test]
    fn seq_loop_or_seq_tau() {
        let tree = seq!(
            tloop!(
                or!(activity!("a"), seq!(activity!("b1"), activity!("b2"))),
                tau!()
            ),
            activity!("c")
        );

        const A: usize = 0;
        const B1: usize = 1;
        const B2: usize = 2;
        const TAU: usize = 3;
        const C: usize = 4;
        const FIN: usize = 5;

        let mut state = get_initial_state(&tree).unwrap();
        assert_eq!(get_enabled_transitions(&tree, &state), [A, B1]);

        execute_transition(&tree, &mut state, A).unwrap();
        assert_eq!(get_enabled_transitions(&tree, &state), [B1, TAU, C]);

        execute_transition(&tree, &mut state, B1).unwrap();
        assert_eq!(get_enabled_transitions(&tree, &state), [B2]);

        execute_transition(&tree, &mut state, B2).unwrap();
        assert_eq!(get_enabled_transitions(&tree, &state), [TAU, C]);

        execute_transition(&tree, &mut state, C).unwrap();
        assert_eq!(get_enabled_transitions(&tree, &state), [FIN]);

        execute_transition(&tree, &mut state, FIN).unwrap();
        assert_eq!(get_enabled_transitions(&tree, &state), [0; 0]);
    }

    #[test]
    fn seq_loop_or_seq() {
        let tree = seq!(
            tloop!(or!(activity!("a"), seq!(activity!("b1"), activity!("b2")))),
            activity!("c")
        );

        const A: usize = 0;
        const B1: usize = 1;
        const B2: usize = 2;
        const C: usize = 3;
        const FIN: usize = 4;

        let mut state = get_initial_state(&tree).unwrap();
        assert_eq!(get_enabled_transitions(&tree, &state), [A, B1]);

        execute_transition(&tree, &mut state, A).unwrap();
        assert_eq!(get_enabled_transitions(&tree, &state), [A, B1, C]);

        execute_transition(&tree, &mut state, B1).unwrap();
        assert_eq!(get_enabled_transitions(&tree, &state), [B2]);

        execute_transition(&tree, &mut state, B2).unwrap();
        assert_eq!(get_enabled_transitions(&tree, &state), [A, B1, C]);

        execute_transition(&tree, &mut state, C).unwrap();
        assert_eq!(get_enabled_transitions(&tree, &state), [FIN]);

        execute_transition(&tree, &mut state, FIN).unwrap();
        assert_eq!(get_enabled_transitions(&tree, &state), [0; 0]);
    }
}
