use crate::{
    ActivityKey,
    ebi_objects::{
        process_tree::{ProcessTree, TreeNode},
        process_tree_markup_language::ProcessTreeMarkupLanguage,
        stochastic_process_tree::StochasticProcessTree,
    },
};

impl From<ProcessTreeMarkupLanguage> for ProcessTree {
    fn from(value: ProcessTreeMarkupLanguage) -> Self {
        value.tree
    }
}

impl From<StochasticProcessTree> for ProcessTree {
    fn from(value: StochasticProcessTree) -> Self {
        Self::new(value.activity_key, value.tree)
    }
}

impl From<(ActivityKey, Vec<TreeNode>)> for ProcessTree {
    fn from(value: (ActivityKey, Vec<TreeNode>)) -> Self {
        Self::new(value.0, value.1)
    }
}

#[macro_export]
macro_rules! tau {
    () => {
        $crate::ProcessTree::from(($crate::ActivityKey::default(), vec![$crate::TreeNode::Tau]))
    };
}
pub use tau;

#[macro_export]
macro_rules! activity {
    ($a:expr) => {{
        let mut activity_key = $crate::ActivityKey::default();
        let act = activity_key.process_activity($a);
        $crate::ProcessTree::from((activity_key, vec![$crate::TreeNode::Activity(act)]))
    }};
}
pub use activity;

#[macro_export]
macro_rules! xor {
    ($($opt:expr),+) => {{
        let mut activity_key = $crate::ActivityKey::default();
        let mut tree = vec![];
        let mut len = 0;
        $(
            let opt = $opt;
            let translator = $crate::ActivityKeyTranslator::new($crate::ebi_activity_key::HasActivityKey::activity_key(&opt), &mut activity_key);
            tree.extend(opt.tree.into_iter().map(|node| if let $crate::TreeNode::Activity(act) = node {$crate::TreeNode::Activity(translator.translate_activity(&act))} else {node}));
            len += 1;
        )+
        tree.insert(0, $crate::TreeNode::Operator($crate::TreeOperator::Xor, len));
        $crate::ProcessTree::from((activity_key, tree))
    }};
}
pub use xor;

#[macro_export]
macro_rules! seq {
    ($($opt:expr),+) => {{
        let mut activity_key = $crate::ActivityKey::default();
        let mut tree = vec![];
        let mut len = 0;
        $(
            let opt = $opt;
            let translator = $crate::ActivityKeyTranslator::new($crate::ebi_activity_key::HasActivityKey::activity_key(&opt), &mut activity_key);
            tree.extend(opt.tree.into_iter().map(|node| if let $crate::TreeNode::Activity(act) = node {$crate::TreeNode::Activity(translator.translate_activity(&act))} else {node}));
            len += 1;
        )+
        tree.insert(0, $crate::TreeNode::Operator($crate::TreeOperator::Sequence, len));
        $crate::ProcessTree::from((activity_key, tree))
    }};
}
pub use seq;

#[macro_export]
macro_rules! con {
    ($($opt:expr),+) => {{
        let mut activity_key = $crate::ActivityKey::default();
        let mut tree = vec![];
        let mut len = 0;
        $(
            let opt = $opt;
            let translator = $crate::ActivityKeyTranslator::new($crate::ebi_activity_key::HasActivityKey::activity_key(&opt), &mut activity_key);
            tree.extend(opt.tree.into_iter().map(|node| if let $crate::TreeNode::Activity(act) = node {$crate::TreeNode::Activity(translator.translate_activity(&act))} else {node}));
            len += 1;
        )+
        tree.insert(0, $crate::TreeNode::Operator($crate::TreeOperator::Concurrent, len));
        $crate::ProcessTree::from((activity_key, tree))
    }};
}
pub use con;

#[macro_export]
macro_rules! or {
    ($($opt:expr),+) => {{
        let mut activity_key = $crate::ActivityKey::default();
        let mut tree = vec![];
        let mut len = 0;
        $(
            let opt = $opt;
            let translator = $crate::ActivityKeyTranslator::new($crate::ebi_activity_key::HasActivityKey::activity_key(&opt), &mut activity_key);
            tree.extend(opt.tree.into_iter().map(|node| if let $crate::TreeNode::Activity(act) = node {$crate::TreeNode::Activity(translator.translate_activity(&act))} else {node}));
            len += 1;
        )+
        tree.insert(0, $crate::TreeNode::Operator($crate::TreeOperator::Or, len));
        $crate::ProcessTree::from((activity_key, tree))
    }};
}
pub use or;

#[macro_export]
macro_rules! int {
    ($($opt:expr),+) => {{
        let mut activity_key = $crate::ActivityKey::default();
        let mut tree = vec![];
        let mut len = 0;
        $(
            let opt = $opt;
            let translator = $crate::ActivityKeyTranslator::new($crate::ebi_activity_key::HasActivityKey::activity_key(&opt), &mut activity_key);
            tree.extend(opt.tree.into_iter().map(|node| if let $crate::TreeNode::Activity(act) = node {$crate::TreeNode::Activity(translator.translate_activity(&act))} else {node}));
            len += 1;
        )+
        tree.insert(0, $crate::TreeNode::Operator($crate::TreeOperator::Interleaved, len));
        $crate::ProcessTree::from((activity_key, tree))
    }};
}
pub use int;

#[macro_export]
macro_rules! tloop {
    ($($opt:expr),+) => {{
        let mut activity_key = $crate::ActivityKey::default();
        let mut tree = vec![];
        let mut len = 0;
        $(
            let opt = $opt;
            let translator = $crate::ActivityKeyTranslator::new($crate::ebi_activity_key::HasActivityKey::activity_key(&opt), &mut activity_key);
            tree.extend(opt.into_iter().map(|node| if let $crate::TreeNode::Activity(act) = node {$crate::TreeNode::Activity(translator.translate_activity(&act))} else {*node}));
            len += 1;
        )+
        tree.insert(0, $crate::TreeNode::Operator($crate::TreeOperator::Loop, len));
        $crate::ProcessTree::from((activity_key, tree))
    }};
}
pub use tloop;

#[cfg(test)]
mod tests {
    #[test]
    fn macros() {
        let _ = tloop!(int!(con!(seq!(or!(xor!(tau!(), activity!("a")))))));
    }
}
