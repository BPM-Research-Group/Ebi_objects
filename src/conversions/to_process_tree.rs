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

#[macro_export]
macro_rules! tau {
    () => {
        ProcessTree::from((ActivityKey::new(), vec![Node::Tau]))
    };
}
pub use tau;

#[macro_export]
macro_rules! activity {
    ($a:expr) => {{
        let mut activity_key = ActivityKey::new();
        let act = activity_key.process_activity($a);
        ProcessTree::from((activity_key, vec![Node::Activity(act)]))
    }};
}
pub use activity;

#[macro_export]
macro_rules! xor {
    ($($opt:expr),+) => {{
        let mut activity_key = ActivityKey::new();
        let mut tree = vec![];
        let mut len = 0;
        $(
            let opt = $opt;
            let translator = ActivityKeyTranslator::new(&opt.activity_key, &mut activity_key);
            tree.extend(opt.tree.into_iter().map(|node| if let Node::Activity(act) = node {Node::Activity(translator.translate_activity(&act))} else {node}));
            len += 1;
        )+
        tree.insert(0, Node::Operator(Operator::Xor, len));
        ProcessTree::from((activity_key, tree))
    }};
}
pub use xor;

#[macro_export]
macro_rules! seq {
    ($($opt:expr),+) => {{
        let mut activity_key = ActivityKey::new();
        let mut tree = vec![];
        let mut len = 0;
        $(
            let opt = $opt;
            let translator = ActivityKeyTranslator::new(&opt.activity_key, &mut activity_key);
            tree.extend(opt.tree.into_iter().map(|node| if let Node::Activity(act) = node {Node::Activity(translator.translate_activity(&act))} else {node}));
            len += 1;
        )+
        tree.insert(0, Node::Operator(Operator::Sequence, len));
        ProcessTree::from((activity_key, tree))
    }};
}
pub use seq;

#[macro_export]
macro_rules! con {
    ($($opt:expr),+) => {{
        let mut activity_key = ActivityKey::new();
        let mut tree = vec![];
        let mut len = 0;
        $(
            let opt = $opt;
            let translator = ActivityKeyTranslator::new(&opt.activity_key, &mut activity_key);
            tree.extend(opt.tree.into_iter().map(|node| if let Node::Activity(act) = node {Node::Activity(translator.translate_activity(&act))} else {node}));
            len += 1;
        )+
        tree.insert(0, Node::Operator(Operator::Concurrent, len));
        ProcessTree::from((activity_key, tree))
    }};
}
pub use con;

#[macro_export]
macro_rules! or {
    ($($opt:expr),+) => {{
        let mut activity_key = ActivityKey::new();
        let mut tree = vec![];
        let mut len = 0;
        $(
            let opt = $opt;
            let translator = ActivityKeyTranslator::new(&opt.activity_key, &mut activity_key);
            tree.extend(opt.tree.into_iter().map(|node| if let Node::Activity(act) = node {Node::Activity(translator.translate_activity(&act))} else {node}));
            len += 1;
        )+
        tree.insert(0, Node::Operator(Operator::Or, len));
        ProcessTree::from((activity_key, tree))
    }};
}
pub use or;

#[macro_export]
macro_rules! int {
    ($($opt:expr),+) => {{
        let mut activity_key = ActivityKey::new();
        let mut tree = vec![];
        let mut len = 0;
        $(
            let opt = $opt;
            let translator = ActivityKeyTranslator::new(&opt.activity_key, &mut activity_key);
            tree.extend(opt.tree.into_iter().map(|node| if let Node::Activity(act) = node {Node::Activity(translator.translate_activity(&act))} else {node}));
            len += 1;
        )+
        tree.insert(0, Node::Operator(Operator::Interleaved, len));
        ProcessTree::from((activity_key, tree))
    }};
}
pub use int;

#[macro_export]
macro_rules! tloop {
    ($($opt:expr),+) => {{
        let mut activity_key = ActivityKey::new();
        let mut tree = vec![];
        let mut len = 0;
        $(
            let opt = $opt;
            let translator = ActivityKeyTranslator::new(&opt.activity_key, &mut activity_key);
            tree.extend(opt.tree.into_iter().map(|node| if let Node::Activity(act) = node {Node::Activity(translator.translate_activity(&act))} else {node}));
            len += 1;
        )+
        tree.insert(0, Node::Operator(Operator::Loop, len));
        ProcessTree::from((activity_key, tree))
    }};
}
pub use tloop;