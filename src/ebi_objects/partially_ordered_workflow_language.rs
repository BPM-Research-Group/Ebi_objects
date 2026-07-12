use crate::{
    EbiObject, Exportable, Graphable, Importable, Infoable, json,
    traits::importable::{ImporterParameter, ImporterParameterValues},
};
use anyhow::{Context, Result, anyhow};
#[cfg(any(test, feature = "testactivities"))]
use ebi_activity_key::TestActivityKey;
use ebi_activity_key::{Activity, ActivityKey, ActivityKeyTranslator, TranslateActivityKey};
use ebi_bpmn::ebi_arithmetic::Fraction;
use ebi_derive::ActivityKey;
use serde_json::{Map, Value};
use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
};

pub const HEADER_FORMAT: &str = "format";
pub const HEADER_FORMAT_VALUE: &str = "powl-json";
pub const HEADER_FORMAT_VERSION: &str = "version";
pub const HEADER_FORMAT_VERSION_VALUE: &str = "1.0";

#[derive(ActivityKey, Clone, Debug)]
pub struct PartiallyOrderedWorkflowLanguage {
    pub activity_key: ActivityKey,
    pub root_model: Model,
    pub lifecycle_key: ActivityKey,
    pub resource_key: ActivityKey,
    pub role_key: ActivityKey,
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

        //verify format
        if json::read_field_string(&json, HEADER_FORMAT)? == HEADER_FORMAT_VALUE {
            return Err(anyhow!(
                "Expected string {} with value {}.",
                HEADER_FORMAT,
                HEADER_FORMAT_VALUE
            ));
        }

        //verify version
        if json::read_field_string(&json, HEADER_FORMAT_VERSION)? == HEADER_FORMAT_VERSION_VALUE {
            return Err(anyhow!(
                "Expected string {} with value {}.",
                HEADER_FORMAT_VERSION,
                HEADER_FORMAT_VERSION_VALUE
            ));
        }

        //read root model
        let json_root_model = json::read_field_object(&json, "model")
            .with_context(|| anyhow!("Could not read root model."))?;

        let mut activity_key = ActivityKey::new();
        let mut lifecycle_key = ActivityKey::new();
        let mut resource_key = ActivityKey::new();
        let mut role_key = ActivityKey::new();
        let root_model = import_model(
            json_root_model,
            &mut activity_key,
            &mut lifecycle_key,
            &mut resource_key,
            &mut role_key,
            true,
        )
        .with_context(|| anyhow!("Reading root model."))?;

        Ok(Self {
            activity_key,
            root_model,
            lifecycle_key,
            resource_key,
            role_key,
        })
    }
}

fn import_model(
    json_model: &Map<String, Value>,
    activity_key: &mut ActivityKey,
    lifecycle_key: &mut ActivityKey,
    resource_key: &mut ActivityKey,
    role_key: &mut ActivityKey,
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
            lifecycle_key,
            resource_key,
            role_key,
        )
        .with_context(|| anyhow!("Reading activity model with id `{:?}`", id)),
        "partial_order" => import_partial_order(
            json_model,
            activity_key,
            id.clone(),
            skippable,
            repeatable,
            lifecycle_key,
            resource_key,
            role_key,
        )
        .with_context(|| anyhow!("Reading partial order model with id `{:?}`", id)),
        "choice_graph" => import_choice_graph(
            json_model,
            activity_key,
            id.clone(),
            skippable,
            repeatable,
            lifecycle_key,
            resource_key,
            role_key,
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
    lifecycle_key: &mut ActivityKey,
    resource_key: &mut ActivityKey,
    role_key: &mut ActivityKey,
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
            .map(|x| resource_key.process_activity(&x));

        //read role
        let role = json::read_string_or_null_from_object(json_attributes, "role")?
            .map(|x| role_key.process_activity(&x));

        //read cost
        let cost = json::read_fraction_or_null_from_object(json_attributes, "cost")?;

        //read lifecycle
        let lifecycle = json::read_string_or_null_from_object(json_attributes, "lifecycle")?
            .map(|x| lifecycle_key.process_activity(&x));

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
    lifecycle_key: &mut ActivityKey,
    resource_key: &mut ActivityKey,
    role_key: &mut ActivityKey,
) -> Result<Model> {
    //read basic attributes
    let (name, description) = read_attributes(json_model)
        .with_context(|| anyhow!("Reading attributes of node {:?}.", id))?;

    //read nodes
    let (nodes, id_2_index) = read_nodes(
        json_model,
        activity_key,
        lifecycle_key,
        resource_key,
        role_key,
    )
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
    lifecycle_key: &mut ActivityKey,
    resource_key: &mut ActivityKey,
    role_key: &mut ActivityKey,
) -> Result<Model> {
    //read basic attributes
    let (name, description) = read_attributes(json_model)
        .with_context(|| anyhow!("Reading attributes of node {:?}.", id))?;

    //read nodes
    let (nodes, id_2_index) = read_nodes(
        json_model,
        activity_key,
        lifecycle_key,
        resource_key,
        role_key,
    )
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
    lifecycle_key: &mut ActivityKey,
    resource_key: &mut ActivityKey,
    role_key: &mut ActivityKey,
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
        let node = import_model(
            json_node,
            activity_key,
            lifecycle_key,
            resource_key,
            role_key,
            false,
        )
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

impl Infoable for PartiallyOrderedWorkflowLanguage {}

#[cfg(any(test, feature = "testactivities"))]
impl TestActivityKey for PartiallyOrderedWorkflowLanguage {}

impl Display for PartiallyOrderedWorkflowLanguage {}

impl Graphable for PartiallyOrderedWorkflowLanguage {}

impl Exportable for PartiallyOrderedWorkflowLanguage {}

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
