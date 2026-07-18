use crate::{AutomatonSemantics, AutomatonState};

pub trait StronglyConnectedComponents {
    fn strongly_connected_components(&self) -> Vec<Vec<usize>>;
}

impl<A> StronglyConnectedComponents for A
where
    A: AutomatonSemantics,
{
    fn strongly_connected_components(&self) -> Vec<Vec<usize>> {
        strongly_connected_components(self, self.number_of_states())
    }
}

impl <E> StronglyConnectedComponents for (E, usize) where E: Edges {
    fn strongly_connected_components(&self) -> Vec<Vec<usize>> {
        strongly_connected_components(&self.0, self.1)
    }
}

pub trait Node: Copy + Ord {
    fn to_id(&self) -> usize;
    fn from_id(id: usize) -> Self;
}

impl Node for AutomatonState {
    fn to_id(&self) -> usize {
        self.0
    }

    fn from_id(id: usize) -> Self {
        Self(id)
    }
}

impl Node for usize {
    fn to_id(&self) -> usize {
        *self
    }

    fn from_id(id: usize) -> Self {
        id
    }
}

pub trait Edges {
    type Node: Node;

    fn outgoing_edges_of(&self, source: Self::Node) -> impl Iterator<Item = Self::Node>;
}

impl<N> Edges for Vec<(N, N)>
where
    N: Node,
{
    type Node = N;

    fn outgoing_edges_of(&self, source: Self::Node) -> impl Iterator<Item = Self::Node> {
        let i = match self.binary_search(&(source, Node::from_id(0))) {
            Ok(x) | Err(x) => x,
        };
        let j = match self.binary_search(&(source, Node::from_id(usize::MAX))) {
            Ok(x) | Err(x) => x,
        };
        self[i..j].iter().map(|(_, target)| *target)
    }
}

impl<N> Edges for Vec<Vec<N>>
where
    N: Node,
{
    type Node = N;

    fn outgoing_edges_of(&self, source: Self::Node) -> impl Iterator<Item = Self::Node> {
        self[source.to_id()].iter().copied()
    }
}

impl<A> Edges for A
where
    A: AutomatonSemantics,
{
    type Node = AutomatonState;

    fn outgoing_edges_of(&self, source: Self::Node) -> impl Iterator<Item = Self::Node> {
        self.outgoing_transitions(source)
            .into_iter()
            .map(|transition| self.transition_2_target(transition).unwrap())
    }
}

/// Computes the strongly connected components using Tarjan's algorithm.
/// Derived from the `strongly_connected_components` crate, adapted to not require building two hashmaps.
/// If `edges` is a vector, it must be sorted.
pub fn strongly_connected_components<N, E>(edges: &E, number_of_nodes: usize) -> Vec<Vec<usize>>
where
    N: Node,
    E: Edges<Node = N>,
{
    let mut state = State {
        stack: Vec::with_capacity(number_of_nodes),
        external_node_states: vec![NodeState::Unvisited; number_of_nodes],
        lowlink: vec![0; number_of_nodes],
        next_internal_id: 0,
        sccs: Vec::new(),
    };
    let mut dfs_stack: Vec<DfsStackElement<N>> = (0..number_of_nodes)
        .map(|node_id| DfsStackElement::Entry {
            parent_external_id: None,
            node_external_id: Node::from_id(node_id),
        })
        .collect();
    while let Some(top_element) = dfs_stack.pop() {
        process_dfs_stack(edges, &mut state, &mut dfs_stack, top_element);
    }
    state.sccs
}

fn process_dfs_stack<E, N>(
    edges: &E,
    state: &mut State,
    dfs_stack: &mut Vec<DfsStackElement<N>>,
    element: DfsStackElement<N>,
) where
    N: Node,
    E: Edges<Node = N>,
{
    match element {
        DfsStackElement::Entry {
            parent_external_id,
            node_external_id,
        } => match state.external_node_states[node_external_id.to_id()] {
            NodeState::Unvisited => {
                let node_internal_id = state.next_internal_id;
                state.next_internal_id += 1;
                state.external_node_states[node_external_id.to_id()] = NodeState::OnStack;
                state.stack.push(node_external_id.to_id());
                state.lowlink[node_external_id.to_id()] = node_internal_id;
                dfs_stack.push(DfsStackElement::Exit {
                    parent_external_id,
                    node_external_id,
                    node_internal_id: Node::from_id(node_internal_id),
                });
                dfs_stack.extend(edges.outgoing_edges_of(node_external_id).map(
                    |successor_external_id| DfsStackElement::Entry {
                        parent_external_id: Some(node_external_id),
                        node_external_id: successor_external_id,
                    },
                ));
            }
            NodeState::OnStack => {
                if let Some(parent_id) = parent_external_id {
                    state.lowlink[parent_id.to_id()] = state.lowlink[parent_id.to_id()]
                        .min(state.lowlink[node_external_id.to_id()]);
                }
            }
            NodeState::Removed => (),
        },
        DfsStackElement::Exit {
            parent_external_id,
            node_external_id,
            node_internal_id,
        } => {
            let result = state.lowlink[node_external_id.to_id()];
            if result == node_internal_id.to_id() {
                collect_sccs_from_stack(state, node_external_id.to_id());
            }
            if let Some(parent_id) = parent_external_id {
                state.lowlink[parent_id.to_id()] = state.lowlink[parent_id.to_id()].min(result);
            }
        }
    }
}

fn collect_sccs_from_stack(state: &mut State, lowest_node_id: usize) {
    let mut new_scc = vec![];
    loop {
        let node_id = state
            .stack
            .pop()
            .expect("assertion: the stack can never be empty here");
        assert!(matches!(
            state.external_node_states[node_id],
            NodeState::OnStack
        ));
        state.external_node_states[node_id] = NodeState::Removed;
        new_scc.push(node_id);
        if node_id == lowest_node_id {
            break;
        }
    }
    state.sccs.push(new_scc);
}

#[derive(Copy, Clone)]
enum NodeState {
    Unvisited,
    OnStack,
    Removed,
}

enum DfsStackElement<N> {
    Entry {
        parent_external_id: Option<N>,
        node_external_id: N,
    },
    Exit {
        parent_external_id: Option<N>,
        node_external_id: N,
        node_internal_id: N,
    },
}

struct State {
    stack: Vec<usize>,
    external_node_states: Vec<NodeState>,
    lowlink: Vec<usize>,
    next_internal_id: usize,
    sccs: Vec<Vec<usize>>,
}
