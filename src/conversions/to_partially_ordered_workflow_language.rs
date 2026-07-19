use crate::{
    ProcessTree, StochasticProcessTree, ebi_objects::{
        partially_ordered_workflow_language::{Keys, PartiallyOrderedWorkflowLanguage, PowlNode},
        process_tree::{Node, Operator},
    },
};

impl From<StochasticProcessTree> for PartiallyOrderedWorkflowLanguage {
    fn from(value: StochasticProcessTree) -> Self {
        ProcessTree::from(value).into()
    }
}

impl From<ProcessTree> for PartiallyOrderedWorkflowLanguage {
    fn from(value: ProcessTree) -> Self {
        let tree = tree_2_powl(&value, 0);
        Self {
            activity_key: value.activity_key,
            tree,
            keys: Keys::default(),
        }
    }
}

fn tree_2_powl(tree: &ProcessTree, node: usize) -> Vec<PowlNode> {
    match tree.tree[node] {
        Node::Tau => vec![PowlNode::Activity {
            id: Some(format!("node {}", node)),
            activity: None,
            resource: None,
            role: None,
            cost: None,
            lifecycle: None,
            skippable: false,
            repeatable: false,
            name: None,
            description: None,
        }],
        Node::Activity(activity) => vec![PowlNode::Activity {
            id: Some(format!("node {}", node)),
            activity: Some(activity),
            resource: None,
            role: None,
            cost: None,
            lifecycle: None,
            skippable: false,
            repeatable: false,
            name: None,
            description: None,
        }],
        Node::Operator(Operator::Sequence, number_of_children) => {
            let mut child_nodes = vec![];
            for child in tree.get_children(node) {
                child_nodes.append(&mut tree_2_powl(tree, child));
            }

            let mut result = vec![];
            result.push(PowlNode::PartialOrder {
                id: Some(format!("node {}", node)),
                number_of_children,
                edges: (1..number_of_children).map(|x| (x - 1, x)).collect(),
                skippable: false,
                repeatable: false,
                name: None,
                description: None,
            });

            result.extend(child_nodes);
            result
        }
        Node::Operator(Operator::Xor, number_of_children) => {
            let mut child_nodes = vec![];
            for child in tree.get_children(node) {
                child_nodes.append(&mut tree_2_powl(tree, child));
            }

            let mut result = vec![];
            result.push(PowlNode::ChoiceGraph {
                id: Some(format!("node {}", node)),
                number_of_children,
                edges: vec![],
                start_children: (0..number_of_children).collect(),
                end_children: (0..number_of_children).collect(),
                skippable: false,
                repeatable: false,
                name: None,
                description: None,
            });

            result.extend(child_nodes);
            result
        }
        Node::Operator(Operator::Concurrent, number_of_children) => {
            let mut child_nodes = vec![];
            for child in tree.get_children(node) {
                child_nodes.append(&mut tree_2_powl(tree, child));
            }

            let mut result = vec![];
            result.push(PowlNode::PartialOrder {
                id: Some(format!("node {}", node)),
                number_of_children,
                edges: vec![],
                skippable: false,
                repeatable: false,
                name: None,
                description: None,
            });

            result.extend(child_nodes);
            result
        }
        Node::Operator(Operator::Loop, number_of_children) => {
            let mut child_nodes = vec![];
            for child in tree.get_children(node) {
                child_nodes.append(&mut tree_2_powl(tree, child));
            }

            let mut result = vec![];
            result.push(PowlNode::ChoiceGraph {
                id: Some(format!("node {}", node)),
                number_of_children,
                edges: (1..number_of_children)
                    .map(|x| (0, x))
                    .chain((1..number_of_children).map(|x| (x, 0)))
                    .collect(),
                start_children: vec![0],
                end_children: vec![0],
                skippable: false,
                repeatable: false,
                name: None,
                description: None,
            });

            result.extend(child_nodes);
            result
        }
        Node::Operator(Operator::Or, number_of_children) => {
            //OR(a, b, c) = XOR(AND(A, B?, C?), AND(A?, B, C?), AND(A?, B?, C))

            //recurse
            let mut child_blocks = vec![];
            for child in tree.get_children(node) {
                child_blocks.push(tree_2_powl(tree, child));
            }

            let mut and_blocks = vec![];
            for (child_rank, _) in tree.get_children(node).enumerate() {
                let mut and_block = vec![];
                and_block.push(PowlNode::PartialOrder {
                    id: Some(format!("and block {}", child_rank)),
                    number_of_children,
                    edges: vec![],
                    skippable: false,
                    repeatable: false,
                    name: None,
                    description: None,
                });

                for (child_rank2, _) in tree.get_children(node).enumerate() {
                    if child_rank == child_rank2 {
                        //the `own` child is not skippable
                        and_block.extend(child_blocks[child_rank2].clone());
                    } else {
                        //every other child is skippable
                        let mut x = child_blocks[child_rank2].clone();
                        x[0].set_skippable(true);
                        and_block.extend(x);
                    }
                }
                and_blocks.extend(and_block);
            }

            let mut result = vec![];
            result.push(PowlNode::ChoiceGraph {
                id: Some(format!("node {}", node)),
                number_of_children,
                edges: vec![],
                start_children: (0..number_of_children).collect(),
                end_children: (0..number_of_children).collect(),
                skippable: false,
                repeatable: false,
                name: None,
                description: None,
            });

            result.extend(and_blocks);
            result
        }
        Node::Operator(Operator::Interleaved, number_of_children) => {
            //creates a tree

            //recurse
            let mut child_blocks = vec![];
            for child in tree.get_children(node) {
                child_blocks.push(tree_2_powl(tree, child));
            }

            create_interleaved_child(&child_blocks, &(0..number_of_children).collect())
        }
    }
}

fn create_interleaved_child(
    child_blocks: &Vec<Vec<PowlNode>>,
    children_left: &Vec<usize>,
) -> Vec<PowlNode> {
    //base case: single child left
    if children_left.len() == 1 {
        return child_blocks[children_left[0]].clone();
    }

    //strategy:
    //int(a, b, c)  => choicegraph(
    //  start -> a,
    //  a -> int(b,c),
    //  int(b,c) -> end,
    //  start -> b,
    //  b -> int(a, c), ...)

    //first, choice graph
    let mut result = vec![PowlNode::ChoiceGraph {
        id: format!("choice {:?}", children_left).into(),
        number_of_children: children_left.len() * 2,
        edges: (0..children_left.len())
            .map(|x| (x * 2, x * 2 + 1))
            .collect(),
        start_children: (0..children_left.len()).map(|x| x * 2).collect(),
        end_children: (0..children_left.len()).map(|x| x * 2 + 1).collect(),
        skippable: false,
        repeatable: false,
        name: None,
        description: None,
    }];

    for (i, _) in children_left.iter().enumerate() {
        //remove the current child
        let mut new_children_left = children_left.clone();
        new_children_left.remove(i);

        //first, the child
        result.extend(child_blocks[i].clone());

        //then, the other children (recurse)
        let mut x = create_interleaved_child(child_blocks, &new_children_left);
        x[0].set_id(format!(
            "interleaved node pivot, children left {:?}",
            new_children_left
        ));
        result.extend(x);
    }

    result
}

#[cfg(test)]
mod tests {
    use crate::{
        PartiallyOrderedWorkflowLanguage, ProcessTree,
        ebi_objects::{
            process_tree::{Node, Operator},
            scalable_vector_graphics::ToSVG,
        },
    };
    use ebi_activity_key::ActivityKey;
    use std::fs;

    #[test]
    fn tree_2_powl_int() {
        let mut activity_key = ActivityKey::new();
        let a = activity_key.process_activity("a");
        let b = activity_key.process_activity("b");
        let c = activity_key.process_activity("c");
        let d = activity_key.process_activity("d");
        let e = activity_key.process_activity("e");
        let tree = vec![
            Node::Operator(Operator::Or, 3),
            Node::Operator(Operator::Xor, 2),
            Node::Activity(a),
            Node::Tau,
            Node::Operator(Operator::Sequence, 2),
            Node::Activity(b),
            Node::Activity(c),
            Node::Operator(Operator::Interleaved, 2),
            Node::Activity(d),
            Node::Activity(e),
        ];
        let ptree = ProcessTree::from((activity_key, tree));

        let powl = PartiallyOrderedWorkflowLanguage::from(ptree);

        let powl2 = powl
            .to_string()
            .parse::<PartiallyOrderedWorkflowLanguage>()
            .unwrap();

        powl.to_svg().unwrap();
    }
}
