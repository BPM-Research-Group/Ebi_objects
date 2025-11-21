use crate::{ActivityKey, ebi_objects::{
    process_tree::{Node, ProcessTree}, process_tree_markup_language::ProcessTreeMarkupLanguage,
    stochastic_process_tree::StochasticProcessTree,
}};

impl From<ProcessTreeMarkupLanguage> for ProcessTree {
    fn from(value: ProcessTreeMarkupLanguage) -> Self {
        value.tree
    }
}

impl From<StochasticProcessTree> for ProcessTree {
    fn from(value: StochasticProcessTree) -> Self {
        Self {
            activity_key: value.activity_key,
            tree: value.tree,
            transition2node: value.transition2node,
        }
    }
}

impl From<(ActivityKey, Vec<Node>)> for ProcessTree {
    fn from(value: (ActivityKey, Vec<Node>)) -> Self {
        let mut transition2node = vec![];
        for (node_index, node) in value.1.iter().enumerate() {
            match node {
                Node::Tau | Node::Activity(_) => {
                    transition2node.push(node_index);
                }
                Node::Operator(_, _) => {}
            }
        }

        Self {
            activity_key: value.0,
            tree: value.1,
            transition2node: transition2node,
        }
    }
}