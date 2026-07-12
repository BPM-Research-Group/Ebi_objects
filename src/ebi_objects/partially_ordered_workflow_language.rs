use crate::{
    EbiObject, Exportable, Graphable, Importable, Infoable, json,
    traits::{
        graphable,
        importable::{ImporterParameter, ImporterParameterValues, from_string},
    },
};
use anyhow::{Context, Error, Result, anyhow};
#[cfg(any(test, feature = "testactivities"))]
use ebi_activity_key::TestActivityKey;
use ebi_activity_key::{
    Activity, ActivityKey, ActivityKeyTranslator, HasActivityKey, TranslateActivityKey,
};
use ebi_bpmn::ebi_arithmetic::Fraction;
use ebi_derive::ActivityKey;
use layout::{adt::dag::NodeHandle, core::base::Orientation, topo::layout::VisualGraph};
use serde_json::{Map, Value, json};
use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
};

pub const HEADER_FORMAT: &str = "format";
pub const HEADER_FORMAT_VALUE: &str = "powl-json";
pub const HEADER_FORMAT_VERSION: &str = "format_version";
pub const HEADER_FORMAT_VERSION_VALUE: &str = "1.0";

#[derive(ActivityKey, Clone, Debug)]
pub struct PartiallyOrderedWorkflowLanguage {
    pub activity_key: ActivityKey,
    pub root_model: Model,
    pub keys: Keys,
}

impl PartiallyOrderedWorkflowLanguage {
    pub fn number_of_nodes(&self) -> usize {
        self.root_model.len_recursive()
    }

    pub fn nodes(&'_ self) -> ModelIterator<'_> {
        self.root_model.nodes()
    }
}

impl Importable for PartiallyOrderedWorkflowLanguage {
    const FILE_FORMAT_SPECIFICATION_LATEX: &str =
        &"A partially ordered workflow language is a JSON structure.";

    const IMPORTER_PARAMETERS: &[ImporterParameter] = &[];

    fn import_as_object(
        reader: &mut dyn std::io::BufRead,
        parameter_values: &ImporterParameterValues,
    ) -> Result<EbiObject> {
        Ok(EbiObject::PartiallyOrderedWorkflowLanguage(Self::import(
            reader,
            parameter_values,
        )?))
    }

    fn import(reader: &mut dyn std::io::BufRead, _: &ImporterParameterValues) -> Result<Self>
    where
        Self: Sized,
    {
        let json: Value = serde_json::from_reader(reader)?;
        let json = json::read_object(&json)
            .with_context(|| anyhow!("Expected the root element to be an object."))?;

        //verify format
        if json::read_string_from_object(&json, HEADER_FORMAT)? != HEADER_FORMAT_VALUE {
            return Err(anyhow!(
                "Expected string {} with value {}.",
                HEADER_FORMAT,
                HEADER_FORMAT_VALUE
            ));
        }

        //verify version
        if json::read_string_from_object(&json, HEADER_FORMAT_VERSION)?
            != HEADER_FORMAT_VERSION_VALUE
        {
            return Err(anyhow!(
                "Expected string {} with value {}.",
                HEADER_FORMAT_VERSION,
                HEADER_FORMAT_VERSION_VALUE
            ));
        }

        //read root model
        let json_root_model = json::read_object_from_object(&json, "model")
            .with_context(|| anyhow!("Could not read root model."))?;

        let mut activity_key = ActivityKey::new();
        let mut keys = Keys::default();
        let root_model = import_model(json_root_model, &mut activity_key, &mut keys, true)
            .with_context(|| anyhow!("Reading root model."))?;

        Ok(Self {
            activity_key,
            root_model,
            keys,
        })
    }
}
from_string!(PartiallyOrderedWorkflowLanguage);

fn import_model(
    json_model: &Map<String, Value>,
    activity_key: &mut ActivityKey,
    keys: &mut Keys,
    is_root: bool,
) -> Result<Model> {
    //read id
    let id = if !is_root {
        let id = json::read_string_from_object(json_model, "id").with_context(|| {
            anyhow!("As this model is not at the root, a field `id` is mandatory, but missing.")
        })?;
        Some(id.clone())
    } else {
        None
    };

    //read skippable
    let skippable = json::read_bool_or_null_from_object(json_model, "skippable")?.unwrap_or(false);

    //read repeatable
    let repeatable =
        json::read_bool_or_null_from_object(json_model, "repeatable")?.unwrap_or(false);

    //read type
    match json::read_string_from_object(json_model, "type")
        .with_context(|| anyhow!("Could not read type of model."))?
        .as_str()
    {
        "activity" => import_activity(
            json_model,
            activity_key,
            id.clone(),
            skippable,
            repeatable,
            keys,
        )
        .with_context(|| anyhow!("Reading activity model with id `{:?}`", id)),
        "partial_order" => import_partial_order(
            json_model,
            activity_key,
            id.clone(),
            skippable,
            repeatable,
            keys,
        )
        .with_context(|| anyhow!("Reading partial order model with id `{:?}`", id)),
        "choice_graph" => import_choice_graph(
            json_model,
            activity_key,
            id.clone(),
            skippable,
            repeatable,
            keys,
        )
        .with_context(|| anyhow!("Reading choice graph model with id `{:?}`", id)),
        x => return Err(anyhow!("Type {} is not recognised as a model type.", x)),
    }
}

fn import_activity(
    json_model: &Map<String, Value>,
    activity_key: &mut ActivityKey,
    id: Option<String>,
    skippable: bool,
    repeatable: bool,
    keys: &mut Keys,
) -> Result<Model> {
    //read the attributes
    let (name, description, resource, role, cost, lifecycle) = if let Some(json_attributes) =
        json::read_object_or_null_from_object(json_model, "attributes")?
    {
        //read name
        let name = json::read_string_or_null_from_object(json_attributes, "name")?;

        //read description
        let description = json::read_string_or_null_from_object(json_attributes, "description")?;

        //read resource
        let resource = json::read_string_or_null_from_object(json_attributes, "resource")?
            .map(|x| keys.resource_key.process_activity(&x));

        //read role
        let role = json::read_string_or_null_from_object(json_attributes, "role")?
            .map(|x| keys.role_key.process_activity(&x));

        //read cost
        let cost = json::read_fraction_or_null_from_object(json_attributes, "cost")?;

        //read lifecycle
        let lifecycle = json::read_string_or_null_from_object(json_attributes, "lifecycle")?
            .map(|x| keys.lifecycle_key.process_activity(&x));

        (name, description, resource, role, cost, lifecycle)
    } else {
        (None, None, None, None, None, None)
    };

    //read the label
    let activity = if let Some(label) = json::read_string_or_null_from_object(json_model, "label")
        .with_context(|| {
        anyhow!(
            "Could not read the label of the activity. Name is `{:?}`.",
            name
        )
    })? {
        Some(activity_key.process_activity(&label))
    } else {
        None
    };

    Ok(Model::Activity {
        id,
        activity,
        skippable,
        repeatable,
        name,
        description,
        role,
        resource,
        cost,
        lifecycle,
    })
}

fn import_partial_order(
    json_model: &Map<String, Value>,
    activity_key: &mut ActivityKey,
    id: Option<String>,
    skippable: bool,
    repeatable: bool,
    keys: &mut Keys,
) -> Result<Model> {
    //read basic attributes
    let (name, description) = read_attributes(json_model)
        .with_context(|| anyhow!("Reading attributes of node {:?}.", id))?;

    //read nodes
    let (nodes, id_2_index) = read_nodes(json_model, activity_key, keys)
        .with_context(|| anyhow!("Reading nodes of node {:?}.", id))?;

    //read edges
    let mut edges = HashSet::new();
    for (edge_index, json_edge) in json::read_list_from_object(json_model, "edges")
        .with_context(|| anyhow!("Field `edges` missing."))?
        .into_iter()
        .enumerate()
    {
        //transform to object
        let json_edge = json::read_object(json_edge)
            .with_context(|| anyhow!("Edge {} should be an object.", edge_index))?;

        //read source
        let source_id = json::read_string_from_object(json_edge, "source")
            .with_context(|| anyhow!("Expected `source` on edge {}.", edge_index))?;
        let source = *id_2_index.get(source_id).ok_or_else(|| {
            anyhow!(
                "Non-existing source `{}` of edge `{}` found.",
                source_id,
                edge_index
            )
        })?;

        //read target
        let target_id = json::read_string_from_object(json_edge, "target")
            .with_context(|| anyhow!("Expected `target` on edge {}.", edge_index))?;
        let target = *id_2_index.get(target_id).ok_or_else(|| {
            anyhow!(
                "Non-existing target `{}` of edge `{}` found.",
                target_id,
                edge_index
            )
        })?;

        //insert the edge and check that it's not inserted twice
        if !edges.insert((source, target)) {
            return Err(anyhow!(
                "Edge from `{:?}` to `{:?}` is duplicate.",
                nodes[source].id(),
                nodes[target].id()
            ));
        }
    }
    let mut edges = edges.into_iter().collect::<Vec<_>>();
    edges.sort();

    //TODO: detect cycles

    //TODO: detect transitive duplication

    Ok(Model::PartialOrder {
        id,
        nodes,
        edges,
        skippable,
        repeatable,
        name,
        description,
    })
}

fn import_choice_graph(
    json_model: &Map<String, Value>,
    activity_key: &mut ActivityKey,
    id: Option<String>,
    skippable: bool,
    repeatable: bool,
    keys: &mut Keys,
) -> Result<Model> {
    //read basic attributes
    let (name, description) = read_attributes(json_model)
        .with_context(|| anyhow!("Reading attributes of node {:?}.", id))?;

    //read nodes
    let (nodes, id_2_index) = read_nodes(json_model, activity_key, keys)
        .with_context(|| anyhow!("Reading nodes of node {:?}.", id))?;

    //check for reserved ids
    if id_2_index.contains_key("@start") {
        return Err(anyhow!(
            "The id `@start` is reserved but is used for a sub-model of node {:?}.",
            id
        ));
    }
    if id_2_index.contains_key("@end") {
        return Err(anyhow!(
            "The id `@end` is reserved but is used for a sub-model of node {:?}.",
            id
        ));
    }

    //read edges
    let mut edges = HashSet::new();
    let mut start_nodes = HashSet::new();
    let mut end_nodes = HashSet::new();
    for (edge_index, json_edge) in json::read_list_from_object(json_model, "edges")
        .with_context(|| anyhow!("Field `edges` missing."))?
        .into_iter()
        .enumerate()
    {
        //transform to object
        let json_edge = json::read_object(json_edge)
            .with_context(|| anyhow!("Edge {} should be an object.", edge_index))?;

        //read source
        let source_id = json::read_string_from_object(json_edge, "source")
            .with_context(|| anyhow!("Expected `source` on edge {}.", edge_index))?;
        let source = if source_id == "@start" {
            None
        } else {
            Some(*id_2_index.get(source_id).ok_or_else(|| {
                anyhow!(
                    "Non-existing source `{}` of edge `{}` found.",
                    source_id,
                    edge_index
                )
            })?)
        };

        //read target
        let target_id = json::read_string_from_object(json_edge, "target")
            .with_context(|| anyhow!("Expected `target` on edge {}.", edge_index))?;
        let target = if target_id == "@end" {
            None
        } else {
            Some(*id_2_index.get(target_id).ok_or_else(|| {
                anyhow!(
                    "Non-existing target `{}` of edge `{}` found.",
                    target_id,
                    edge_index
                )
            })?)
        };

        match (source, target) {
            (None, None) => {
                return Err(anyhow!("Start -> end edge detected in node {:?}", id));
            }
            (None, Some(target)) => {
                //start edge

                //insert the edge and check that it's not inserted twice
                if !start_nodes.insert(target) {
                    return Err(anyhow!(
                        "Start edge to `{:?}` is duplicate in node {:?}.",
                        nodes[target].id(),
                        id
                    ));
                }
            }
            (Some(source), None) => {
                //start edge

                //insert the edge and check that it's not inserted twice
                if !end_nodes.insert(source) {
                    return Err(anyhow!(
                        "End edge from `{:?}` is duplicate in node {:?}.",
                        nodes[source].id(),
                        id
                    ));
                }
            }
            (Some(source), Some(target)) => {
                //normal edge

                //check for self-edges
                if source == target {
                    return Err(anyhow!(
                        "A self-edge from and to `{}` was found in node {:?}.",
                        source_id,
                        id
                    ));
                }

                //insert the edge and check that it's not inserted twice
                if !edges.insert((source, target)) {
                    return Err(anyhow!(
                        "Edge from `{:?}` to `{:?}` is duplicate in node {:?}.",
                        nodes[source].id(),
                        nodes[target].id(),
                        id
                    ));
                }
            }
        }
    }
    let mut edges = edges.into_iter().collect::<Vec<_>>();
    edges.sort();
    let mut start_nodes = start_nodes.into_iter().collect::<Vec<_>>();
    start_nodes.sort();
    let mut end_nodes = end_nodes.into_iter().collect::<Vec<_>>();
    end_nodes.sort();

    //check connectivity

    Ok(Model::ChoiceGraph {
        id,
        nodes,
        edges,
        start_nodes,
        end_nodes,
        skippable,
        repeatable,
        name,
        description,
    })
}

fn read_attributes(json_model: &Map<String, Value>) -> Result<(Option<String>, Option<String>)> {
    //read the attributes
    if let Some(json_attributes) = json::read_object_or_null_from_object(json_model, "attributes")?
    {
        //read name
        let name = json::read_string_or_null_from_object(json_attributes, "name")?;

        //read description
        let description = json::read_string_or_null_from_object(json_attributes, "description")?;

        Ok((name, description))
    } else {
        Ok((None, None))
    }
}

fn read_nodes(
    json_model: &Map<String, Value>,
    activity_key: &mut ActivityKey,
    keys: &mut Keys,
) -> Result<(Vec<Model>, HashMap<String, usize>)> {
    let mut id_2_index = HashMap::new();
    let mut nodes = vec![];
    for (node_index, json_node) in json::read_list_from_object(json_model, "nodes")
        .with_context(|| anyhow!("Reading `nodes` as a list."))?
        .into_iter()
        .enumerate()
    {
        //transform to object
        let json_node = json::read_object(json_node)
            .with_context(|| anyhow!("For node {}, expected an object.", node_index))?;

        //recurse
        let node = import_model(json_node, activity_key, keys, false)
            .with_context(|| anyhow!("Reading node {}.", node_index))?;

        let child_id = node
            .id()
            .ok_or_else(|| anyhow!("Non-root model {} has no id.", node_index))?
            .clone();

        //keep track of id
        if let Some(old_id) = id_2_index.insert(child_id, node_index) {
            //id seen twice
            return Err(anyhow!(
                "The id `{}` was seen twice in the same model.",
                old_id
            ));
        }

        nodes.push(node);
    }

    if nodes.len() < 2 {
        return Err(anyhow!("The model has fewer than two children."));
    }

    Ok((nodes, id_2_index))
}

impl TranslateActivityKey for PartiallyOrderedWorkflowLanguage {
    fn translate_using_activity_key(&mut self, to_activity_key: &mut ActivityKey) {
        let translator = ActivityKeyTranslator::new(&self.activity_key, to_activity_key);

        (0..self.root_model.len_recursive()).for_each(|i| {
            if let Some(model) = self.root_model.get_mut(i) {
                if let Model::Activity {
                    activity: Some(activity),
                    ..
                } = model
                {
                    *activity = translator.translate_activity(&activity);
                }
            }
        });
        self.activity_key = to_activity_key.clone();
    }
}

impl Infoable for PartiallyOrderedWorkflowLanguage {
    fn info(&self, f: &mut impl std::io::Write) -> Result<()> {
        writeln!(f, "Number of nodes\t\t{}", self.number_of_nodes())?;
        writeln!(
            f,
            "Number of activities\t\t{}",
            self.activity_key().get_number_of_activities()
        )?;

        writeln!(f, "")?;
        self.activity_key().info(f)?;

        Ok(writeln!(f, "")?)
    }
}

#[cfg(any(test, feature = "testactivities"))]
impl TestActivityKey for PartiallyOrderedWorkflowLanguage {
    fn test_activity_key(&self) {
        self.nodes().for_each(|node| {
            if let Model::Activity {
                activity: Some(activity),
                ..
            } = node
            {
                self.activity_key().assert_activity_is_of_key(&activity);
            }
        });
    }
}

impl Display for PartiallyOrderedWorkflowLanguage {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let json = serde_json::json!(
            {
                HEADER_FORMAT: HEADER_FORMAT_VALUE,
                HEADER_FORMAT_VERSION: HEADER_FORMAT_VERSION_VALUE,
                "model": self.root_model.to_json(&self.activity_key, &self.keys),
            }
        );
        let x = serde_json::to_string_pretty(&json).unwrap();
        write!(f, "{}", x)
    }
}

impl Graphable for PartiallyOrderedWorkflowLanguage {
    fn to_dot(&self) -> Result<VisualGraph> {
        let mut graph = VisualGraph::new(Orientation::LeftToRight);
        self.root_model.to_dot(self.activity_key(), &mut graph);
        Ok(graph)
    }
}

impl Exportable for PartiallyOrderedWorkflowLanguage {
    fn export_from_object(object: EbiObject, f: &mut dyn std::io::Write) -> Result<()> {
        match object {
            EbiObject::PartiallyOrderedWorkflowLanguage(powl) => powl.export(f),
            _ => Err(anyhow!(
                "Cannot export {} {} as a partially ordered workflow language.",
                object.get_type().get_article(),
                object.get_type()
            )),
        }
    }

    fn export(&self, f: &mut dyn std::io::Write) -> Result<()> {
        Ok(write!(f, "{}", self)?)
    }
}

#[derive(Clone, Debug)]
pub enum Model {
    Activity {
        id: Option<String>,
        activity: Option<Activity>,
        resource: Option<Activity>,
        role: Option<Activity>,
        cost: Option<Fraction>,
        lifecycle: Option<Activity>,
        skippable: bool,
        repeatable: bool,
        name: Option<String>,
        description: Option<String>,
    },
    PartialOrder {
        id: Option<String>,
        nodes: Vec<Model>,
        /// Invariant: sorted by source (first key), then target (second key).
        edges: Vec<(usize, usize)>,
        skippable: bool,
        repeatable: bool,
        name: Option<String>,
        description: Option<String>,
    },
    ChoiceGraph {
        id: Option<String>,
        nodes: Vec<Model>,
        /// Invariant: sorted by source (first key), then target (second key).
        edges: Vec<(usize, usize)>,
        /// Invariant: sorted
        start_nodes: Vec<usize>,
        /// Invariant: sorted
        end_nodes: Vec<usize>,
        skippable: bool,
        repeatable: bool,
        name: Option<String>,
        description: Option<String>,
    },
}

impl Model {
    pub fn id(&self) -> Option<&String> {
        match self {
            Model::Activity { id, .. }
            | Model::PartialOrder { id, .. }
            | Model::ChoiceGraph { id, .. } => id.as_ref(),
        }
    }

    /// Gets a mutable reference to the node with the given recursive index, or None if it does not exist.
    pub fn get_mut(&mut self, recursive_index: usize) -> Option<&mut Self> {
        let mut n = recursive_index;
        get_mut(self, &mut n)
    }

    /// Returns the number of nodes recursively.
    pub fn len_recursive(&self) -> usize {
        match self {
            Self::Activity { .. } => 1,
            Self::PartialOrder { nodes, .. } | Self::ChoiceGraph { nodes, .. } => {
                1 + nodes.iter().map(|node| node.len_recursive()).sum::<usize>()
            }
        }
    }

    pub fn nodes(&'_ self) -> ModelIterator<'_> {
        self.into()
    }

    pub fn to_json(&self, activity_key: &ActivityKey, keys: &Keys) -> Value {
        let mut result = Map::new();
        match self {
            Model::Activity {
                id,
                activity,
                resource,
                role,
                cost,
                lifecycle,
                skippable,
                repeatable,
                name,
                description,
            } => {
                result.insert("type".to_string(), json!("activity"));

                write_common(&mut result, id, skippable, repeatable);

                result.insert(
                    "label".to_string(),
                    if let Some(activity) = activity {
                        json!(activity_key.deprocess_activity(activity))
                    } else {
                        Value::Null
                    },
                );

                {
                    let mut attributes = Map::new();

                    if let Some(name) = name {
                        attributes.insert("name".to_string(), json!(name));
                    }

                    if let Some(description) = description {
                        attributes.insert("description".to_string(), json!(description));
                    }

                    if let Some(cost) = cost {
                        attributes.insert("cost".to_string(), Value::String(cost.to_string()));
                    }

                    if let Some(lifecycle) = lifecycle {
                        attributes.insert(
                            "lifecycle".to_string(),
                            json!(&keys.lifecycle_key.deprocess_activity(lifecycle)),
                        );
                    }

                    if let Some(resource) = resource {
                        attributes.insert(
                            "resource".to_string(),
                            json!(&keys.resource_key.deprocess_activity(resource)),
                        );
                    }

                    if let Some(role) = role {
                        attributes.insert(
                            "role".to_string(),
                            json!(&keys.role_key.deprocess_activity(role)),
                        );
                    }

                    if !attributes.is_empty() {
                        result.insert("attributes".to_string(), Value::Object(attributes));
                    }
                }
            }
            Model::PartialOrder {
                id,
                nodes,
                edges,
                skippable,
                repeatable,
                name,
                description,
            } => {
                result.insert("type".to_string(), json!("partial_order"));

                write_common(&mut result, id, skippable, repeatable);

                write_attributes(&mut result, name, description);

                write_nodes(&mut result, nodes, activity_key, keys);

                result.insert(
                    "edges".to_string(),
                    json!(
                        edges
                            .iter()
                            .map(|(source_index, target_index)| json!({
                                "source": json!(nodes[*source_index].id()),
                                "target": json!(nodes[*target_index].id())
                            }))
                            .collect::<Vec<_>>()
                    ),
                );
            }
            Model::ChoiceGraph {
                id,
                nodes,
                edges,
                start_nodes,
                end_nodes,
                skippable,
                repeatable,
                name,
                description,
            } => {
                result.insert("type".to_string(), json!("choice_graph"));

                write_common(&mut result, id, skippable, repeatable);

                write_attributes(&mut result, name, description);

                write_nodes(&mut result, nodes, activity_key, keys);

                //edges
                let mut json_edges = edges
                    .iter()
                    .map(|(source_index, target_index)| {
                        json!({
                            "source": json!(nodes[*source_index].id()),
                            "target": json!(nodes[*target_index].id())
                        })
                    })
                    .collect::<Vec<_>>();
                //start edges
                json_edges.extend(start_nodes.iter().map(|target_index| {
                    json!({
                        "source": "@start",
                        "target": json!(nodes[*target_index].id())
                    })
                }));
                //end edges
                json_edges.extend(end_nodes.iter().map(|target_index| {
                    json!({
                        "source": json!(nodes[*target_index].id()),
                        "target": "@end"
                    })
                }));
                result.insert("edges".to_string(), json!(json_edges));
            }
        };

        Value::Object(result)
    }

    pub fn to_dot(
        &self,
        activity_key: &ActivityKey,
        graph: &mut VisualGraph,
    ) -> (NodeHandle, NodeHandle) {
        match self {
            Model::Activity { activity: None, .. } => {
                let t = graphable::create_silent_transition(graph, "");
                (t, t)
            }
            Model::Activity {
                activity: Some(activity),
                skippable,
                repeatable,
                ..
            } => {
                let cardinality = match (skippable, repeatable) {
                    (true, true) => "*",
                    (true, false) => "?",
                    (false, true) => "+",
                    (false, false) => "",
                };
                let t = graphable::create_transition(
                    graph,
                    activity_key.get_activity_label(&activity),
                    cardinality,
                );
                (t, t)
            }
            Model::PartialOrder { nodes, edges, .. } => {
                //children
                let index_2_handles = nodes
                    .iter()
                    .map(|node| {
                        let (entry, exit) = node.to_dot(activity_key, graph);
                        let pre_entry = graphable::create_gateway(graph, "+");
                        let post_exit = graphable::create_gateway(graph, "+");
                        graphable::create_edge(graph, &pre_entry, &entry, "");
                        graphable::create_edge(graph, &exit, &post_exit, "");
                        (pre_entry, post_exit)
                    })
                    .collect::<Vec<_>>();

                let mut is_start = vec![true; nodes.len()];
                let mut is_end = vec![true; nodes.len()];

                //edges
                for (source_index, target_index) in edges {
                    let source = index_2_handles[*source_index].1;
                    let target = index_2_handles[*target_index].0;
                    graphable::create_edge(graph, &source, &target, "");

                    is_start[*target_index] = false;
                    is_end[*source_index] = false;
                }

                //start edges
                let start = graphable::create_gateway(graph, "+");
                for (target_index, x) in is_start.into_iter().enumerate() {
                    if x {
                        let target = index_2_handles[target_index].0;
                        graphable::create_edge(graph, &start, &target, "");
                    }
                }

                let end = graphable::create_gateway(graph, "+");
                for (source_index, x) in is_end.into_iter().enumerate() {
                    if x {
                        let source = index_2_handles[source_index].1;
                        graphable::create_edge(graph, &source, &end, "");
                    }
                }

                (start, end)
            }
            Model::ChoiceGraph {
                nodes,
                edges,
                start_nodes,
                end_nodes,
                ..
            } => {
                //children
                let index_2_handles = nodes
                    .iter()
                    .map(|node| {
                        let (entry, exit) = node.to_dot(activity_key, graph);
                        let pre_entry = graphable::create_gateway(graph, "x");
                        let post_exit = graphable::create_gateway(graph, "x");
                        graphable::create_edge(graph, &pre_entry, &entry, "");
                        graphable::create_edge(graph, &exit, &post_exit, "");
                        (pre_entry, post_exit)
                    })
                    .collect::<Vec<_>>();

                //edges
                for (source_index, target_index) in edges {
                    let source = index_2_handles[*source_index].1;
                    let target = index_2_handles[*target_index].0;
                    graphable::create_edge(graph, &source, &target, "");
                }

                //start edges
                let start = graphable::create_gateway(graph, "x");
                for target_index in start_nodes {
                    let target = index_2_handles[*target_index].0;
                    graphable::create_edge(graph, &start, &target, "");
                }

                let end = graphable::create_gateway(graph, "x");
                for source_index in end_nodes {
                    let source = index_2_handles[*source_index].1;
                    graphable::create_edge(graph, &source, &end, "");
                }

                (start, end)
            }
        }
    }
}

fn write_common(
    result: &mut Map<String, Value>,
    id: &Option<String>,
    skippable: &bool,
    repeatable: &bool,
) {
    if let Some(id) = id {
        result.insert("id".to_string(), Value::String(id.to_string()));
    }

    if *skippable {
        result.insert("skippable".to_string(), Value::Bool(*skippable));
    }

    if *repeatable {
        result.insert("repeatable".to_string(), Value::Bool(*repeatable));
    }
}

fn write_attributes(
    result: &mut Map<String, Value>,
    name: &Option<String>,
    description: &Option<String>,
) {
    {
        let mut attributes = Map::new();

        if let Some(name) = name {
            attributes.insert("name".to_string(), json!(name));
        }

        if let Some(description) = description {
            attributes.insert("description".to_string(), json!(description));
        }

        if !attributes.is_empty() {
            result.insert("attributes".to_string(), Value::Object(attributes));
        }
    }
}

fn write_nodes(
    result: &mut Map<String, Value>,
    nodes: &Vec<Model>,
    activity_key: &ActivityKey,
    keys: &Keys,
) {
    result.insert(
        "nodes".to_string(),
        json!(
            nodes
                .iter()
                .map(|node| node.to_json(activity_key, keys))
                .collect::<Vec<_>>()
        ),
    );
}

fn get_mut<'a>(model: &'a mut Model, left: &mut usize) -> Option<&'a mut Model> {
    if *left == 0 {
        return Some(model);
    }

    //skip over self
    *left -= 1;

    match model {
        Model::Activity { .. } => return None,
        Model::PartialOrder { nodes, .. } | Model::ChoiceGraph { nodes, .. } => {
            for child in nodes.iter_mut() {
                if let Some(x) = get_mut(child, left) {
                    return Some(x);
                }
            }
        }
    }

    None
}

pub struct ModelIterator<'a> {
    stack: Vec<&'a Model>,
}

impl<'a> From<&'a Model> for ModelIterator<'a> {
    fn from(value: &'a Model) -> Self {
        Self { stack: vec![value] }
    }
}

impl<'a> Iterator for ModelIterator<'a> {
    type Item = &'a Model;

    fn next(&mut self) -> Option<Self::Item> {
        let next = self.stack.pop()?;
        match next {
            Model::Activity { .. } => {}
            Model::PartialOrder { nodes, .. } | Model::ChoiceGraph { nodes, .. } => {
                self.stack.extend(nodes);
            }
        };
        Some(next)
    }
}

#[derive(Debug, Clone)]
pub struct Keys {
    lifecycle_key: ActivityKey,
    resource_key: ActivityKey,
    role_key: ActivityKey,
}

impl Default for Keys {
    fn default() -> Self {
        Self {
            lifecycle_key: ActivityKey::new(),
            resource_key: ActivityKey::new(),
            role_key: ActivityKey::new(),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::ebi_objects::{
        partially_ordered_workflow_language::PartiallyOrderedWorkflowLanguage,
        scalable_vector_graphics::ToSVG,
    };
    use std::fs;

    #[test]
    fn import() {
        let fin = fs::read_to_string("testfiles/sepsis.powl").unwrap();
        let powl = fin.parse::<PartiallyOrderedWorkflowLanguage>().unwrap();

        let fin2 = powl.to_string();
        let powl2 = fin2.parse::<PartiallyOrderedWorkflowLanguage>().unwrap();

        let fin3 = powl2.to_string();
        let _powl3 = fin3.parse::<PartiallyOrderedWorkflowLanguage>().unwrap();

        assert_eq!(fin2, fin3);
    }

    #[test]
    fn graphable() {
        let fin = fs::read_to_string("testfiles/request_handling.powl").unwrap();
        let powl = fin.parse::<PartiallyOrderedWorkflowLanguage>().unwrap();
        let svg = powl.to_svg().unwrap();

        println!("{}", svg);
    }
}
