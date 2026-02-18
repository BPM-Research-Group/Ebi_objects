#[cfg(any(test, feature = "testactivities"))]
use crate::activity_key::has_activity_key::TestActivityKey;
use crate::{
    Activity, ActivityKey, EbiObject, Graphable, Importable, Infoable, TranslateActivityKey,
    traits::importable::{ImporterParameter, ImporterParameterValues, from_string},
};
use anyhow::{Context, Result, anyhow};
use ebi_derive::ActivityKey;
use quick_xml::{
    Reader,
    events::{BytesEnd, BytesStart, Event},
};
use std::{
    collections::{HashMap, hash_map::Entry},
    fmt::Display,
    io::BufRead,
};

#[derive(Clone, ActivityKey)]
pub struct BusinessProcessModelAndNotation {
    activity_key: ActivityKey,
    pub process: BPMNProcess,
}

#[derive(Clone)]
pub struct BPMNProcess {
    pub element_id_2_index: HashMap<String, usize>,
    pub element_index_2_element: Vec<BPMNElement>,
    pub flow_id_2_index: HashMap<String, usize>,
    pub flow_index_2_flow: Vec<BPMNFlow>,
}

#[derive(Clone, Debug)]
pub enum BPMNElement {
    StartEvent,
    EndEvent,
    Task { activity: Activity },
    ExclusiveGateway,
    InclusiveGateway,
}

impl BPMNElement {}

#[derive(Clone, Copy, Debug)]
enum BPMNElementType {
    StartEvent,
    EndEvent,
    Task,
    ExclusiveGateway,
    InclusiveGateway,
}

impl BPMNElementType {
    fn parse(e: &BytesStart, state: &State) -> Option<Self> {
        if let Some(open_tag) = state.open_tags.iter().last()
            && open_tag == b"bpmn:process"
        {
            match e.name().as_ref() {
                b"bpmn:startEvent" => Some(BPMNElementType::StartEvent),
                b"bpmn:endEvent" => Some(BPMNElementType::EndEvent),
                b"bpmn:task" => Some(BPMNElementType::Task),
                b"bpmn:exclusiveGateway" => Some(BPMNElementType::ExclusiveGateway),
                b"bpmn:inclusiveGateway" => Some(BPMNElementType::InclusiveGateway),
                _ => None,
            }
        } else {
            None
        }
    }

    fn parse_to_element(self, e: &BytesStart, state: &mut State) -> Result<BPMNElement> {
        Ok(match self {
            BPMNElementType::StartEvent => BPMNElement::StartEvent,
            BPMNElementType::EndEvent => BPMNElement::EndEvent,
            BPMNElementType::Task => {
                if let Some(label) = parse_attribute(e, "name") {
                    let activity = state.activity_key.process_activity(&label);
                    BPMNElement::Task { activity }
                } else {
                    return Err(anyhow!("task must have a name"));
                }
            }
            BPMNElementType::ExclusiveGateway => BPMNElement::ExclusiveGateway,
            BPMNElementType::InclusiveGateway => BPMNElement::InclusiveGateway,
        })
    }
}

#[derive(Clone)]
pub enum BPMNFlow {
    SequenceFlow {
        source_element_index: usize,
        target_element_index: usize,
    },
    MessageFlow {
        source_element_index: usize,
        target_element_index: usize,
    },
}

#[derive(Clone, Debug)]
pub enum BPMNFlowType {
    SequenceFlow,
    MessageFlow,
}

impl BPMNFlowType {
    fn parse(e: &BytesStart, state: &State) -> Option<Self> {
        if let Some(open_tag) = state.open_tags.iter().last()
            && open_tag == b"bpmn:process"
        {
            match e.name().as_ref() {
                b"bpmn:sequenceFlow" => Some(BPMNFlowType::SequenceFlow),
                b"bpmn:messageFlow" => Some(BPMNFlowType::MessageFlow),
                _ => None,
            }
        } else {
            None
        }
    }

    fn parse_to_flow(self, e: &BytesStart, state: &mut State) -> Result<BPMNFlow> {
        //parse source and add if necessary
        let new_index = state.element_id_2_index.len();
        let source_ref = parse_attribute(e, "sourceRef")
            .with_context(|| "flow must have an attribute `sourceRef`")?;
        let source_element_index =
            *state
                .element_id_2_index
                .entry(source_ref)
                .or_insert_with(|| {
                    state.element_index_2_element.push(None);
                    new_index
                });

        //parse target and add if necessary
        let new_index = state.element_id_2_index.len();
        let target_ref = parse_attribute(e, "sourceRef")
            .with_context(|| "flow must have an attribute `sourceRef`")?;
        let target_element_index =
            *state
                .element_id_2_index
                .entry(target_ref)
                .or_insert_with(|| {
                    state.element_index_2_element.push(None);
                    new_index
                });

        Ok(match self {
            BPMNFlowType::SequenceFlow => BPMNFlow::SequenceFlow {
                source_element_index,
                target_element_index,
            },
            BPMNFlowType::MessageFlow => BPMNFlow::MessageFlow {
                source_element_index,
                target_element_index,
            },
        })
    }
}

struct State {
    activity_key: ActivityKey,
    open_tags: Vec<Vec<u8>>,
    in_definitions: bool,
    in_process: bool,
    closed_processes: Vec<BPMNProcess>,
    element_id_2_index: HashMap<String, usize>,
    element_index_2_element: Vec<Option<BPMNElement>>,
    flow_id_2_index: HashMap<String, usize>,
    flow_index_2_flow: Vec<BPMNFlow>,
}

impl State {
    fn new() -> Self {
        Self {
            activity_key: ActivityKey::new(),
            open_tags: vec![],
            in_definitions: false,
            in_process: false,
            closed_processes: vec![],
            element_id_2_index: HashMap::new(),
            element_index_2_element: vec![],
            flow_id_2_index: HashMap::new(),
            flow_index_2_flow: vec![],
        }
    }

    fn empty_tag(&mut self, e: &BytesStart) -> Result<()> {
        self.open_tag(e)?;
        self.close_tag(&e.to_end())
    }

    fn open_tag(&mut self, e: &BytesStart) -> Result<()> {
        match (
            e.name().as_ref(),
            BPMNElementType::parse(&e, self),
            BPMNFlowType::parse(&e, self),
        ) {
            (b"bpmn:definitions", _, _) => {
                if self.in_definitions {
                    return Err(anyhow!("cannot nest `bpmn:definitions` tags"));
                } else {
                    self.in_definitions = true;
                }
            }
            (b"bpmn:process", _, _) => {
                if self.in_process {
                    return Err(anyhow!("cannot nest `bpmn:process` tags"));
                } else {
                    self.in_process = true;
                }
            }
            //element tag
            (_, Some(element_type), _) => {
                self.open_element(element_type, &e)
                    .with_context(|| anyhow!("parsing element tag `{:?}`", e.name().as_ref()))?;
            }
            //flow tag
            (_, _, Some(flow_type)) => {
                self.open_flow(flow_type, &e)
                    .with_context(|| anyhow!("parsing tag flow `{:?}`", e.name().as_ref()))?;
            }
            _ => {}
        };

        self.open_tags.push(e.name().as_ref().to_owned());

        // println!("{}", debug(&self.open_tags));

        Ok(())
    }

    fn close_tag(&mut self, e: &BytesEnd) -> Result<()> {
        if let Some(last_tag) = self.open_tags.pop() {
            if last_tag == e.name().as_ref() {
                //closing tag matches last remaining opening tag

                match e.name().as_ref() {
                    b"bpmn:process" => {
                        self.close_process().with_context(|| "close process")?;
                    }
                    _ => {}
                };
                Ok(())
            } else {
                Err(anyhow!(
                    "attempted to close tag `{}` but `{}` was open",
                    String::from_utf8_lossy(e.name().as_ref()),
                    String::from_utf8_lossy(&last_tag)
                ))
            }
        } else {
            Err(anyhow!(
                "attempted to close tag `{}` that was not open",
                String::from_utf8_lossy(e.name().as_ref())
            ))
        }
    }

    fn can_eof(&self) -> Result<()> {
        if let Some(tag) = self.open_tags.iter().next() {
            Err(anyhow!(
                "file ended while tag `{}` was still open",
                String::from_utf8_lossy(&tag)
            ))
        } else {
            Ok(())
        }
    }

    fn open_element(&mut self, element_type: BPMNElementType, e: &BytesStart) -> Result<()> {
        // println!("\topen element {:?}", element_type);

        //create the element
        let element = element_type.parse_to_element(e, self)?;

        //read the id
        if let Some(id) = parse_attribute(e, "id") {
            // println!("\t\tread id `{}`", id);
            let new_index = self.element_id_2_index.len();
            match self.element_id_2_index.entry(id.clone()) {
                Entry::Occupied(oe) => {
                    if let Some(_) = self.element_index_2_element[*oe.get()] {
                        return Err(anyhow!("two elements have the id `{}`", id));
                    } else {
                        //replace the None with the element
                        self.element_index_2_element[*oe.get()] = Some(element);
                    }
                }
                Entry::Vacant(vacant_entry) => {
                    self.element_index_2_element.push(Some(element));
                    vacant_entry.insert(new_index);
                }
            }
        } else {
            return Err(anyhow!("element must have an id"));
        }

        // println!("{:?}", self.element_index_2_element);

        Ok(())
    }

    fn open_flow(&mut self, flow_type: BPMNFlowType, e: &BytesStart) -> Result<()> {
        // println!("\topen flow {:?}", flow_type);

        let flow = flow_type.parse_to_flow(e, self)?;

        //read the id
        if let Some(id) = parse_attribute(e, "id") {
            let new_index = self.flow_index_2_flow.len();
            match self.flow_id_2_index.entry(id.clone()) {
                Entry::Occupied(_) => {
                    return Err(anyhow!("flow with id `{}` is declared twice", id));
                }
                Entry::Vacant(vacant_entry) => {
                    vacant_entry.insert(new_index);
                    self.flow_index_2_flow.push(flow);
                }
            }
        } else {
            return Err(anyhow!("flow must have an id"));
        }

        Ok(())
    }

    fn close_process(&mut self) -> Result<()> {
        // println!("\tclose process");

        let mut element_id_2_index = HashMap::new();
        let mut element_index_2_element = vec![];
        let mut flow_id_2_index = HashMap::new();
        let mut flow_index_2_flow = vec![];

        std::mem::swap(&mut self.element_id_2_index, &mut element_id_2_index);
        std::mem::swap(
            &mut self.element_index_2_element,
            &mut element_index_2_element,
        );
        std::mem::swap(&mut self.flow_id_2_index, &mut flow_id_2_index);
        std::mem::swap(&mut self.flow_index_2_flow, &mut flow_index_2_flow);

        //check whether all mentioned elements have been actually declared
        let mut element_index_2_element_x = Vec::with_capacity(element_index_2_element.len());
        for (index, element) in element_index_2_element.into_iter().enumerate() {
            if let Some(element) = element {
                element_index_2_element_x.push(element);
            } else {
                //None value found
                if let Some((id, _)) = element_id_2_index
                    .iter()
                    .find(|(_, indexx)| **indexx == index)
                {
                    return Err(anyhow!(
                        "id `{}` was mentioned but not declared as an element",
                        id
                    ));
                } else {
                    return Err(anyhow!(
                        "an id was mentioned but not declared as an element"
                    ));
                }
            }
        }

        self.closed_processes.push(BPMNProcess {
            element_id_2_index,
            element_index_2_element: element_index_2_element_x,
            flow_id_2_index,
            flow_index_2_flow,
        });

        Ok(())
    }

    fn to_model(self) -> Result<BusinessProcessModelAndNotation> {
        if let Some(process) = self.closed_processes.into_iter().next() {
            Ok(BusinessProcessModelAndNotation {
                activity_key: self.activity_key,
                process,
            })
        } else {
            Err(anyhow!(
                "no `bpmn:process` tag found nested in a `bpmn:definitions` tag"
            ))
        }
    }
}

// fn debug(inn: &Vec<Vec<u8>>) -> String {
//     inn.iter()
//         .map(|s| String::from_utf8_lossy(s).to_string())
//         .join(", ")
// }

fn parse_attribute(e: &BytesStart, attribute_name: &str) -> Option<String> {
    if let Ok(Some(attribute)) = e.try_get_attribute(attribute_name) {
        Some(
            String::from_utf8_lossy(&attribute.value)
                .as_ref()
                .to_owned(),
        )
    } else {
        None
    }
}

impl Importable for BusinessProcessModelAndNotation {
    const IMPORTER_PARAMETERS: &[ImporterParameter] = &[];

    const FILE_FORMAT_SPECIFICATION_LATEX: &str = "A Business Process Model and Notation (BPMN) model follows the OMG 2.0.2 standard~\\cite{omg2011bpmn}.
    Currently, a sub-set of elements is supported.";

    fn import_as_object(
        reader: &mut dyn BufRead,
        parameter_values: &ImporterParameterValues,
    ) -> Result<EbiObject> {
        Ok(EbiObject::BusinessProcessModelAndNotation(Self::import(
            reader,
            parameter_values,
        )?))
    }

    fn import(
        reader: &mut dyn std::io::BufRead,
        _parameter_values: &crate::traits::importable::ImporterParameterValues,
    ) -> Result<Self>
    where
        Self: Sized,
    {
        let mut xml_reader = Reader::from_reader(reader);
        xml_reader.config_mut().trim_text(true);

        let mut buf = vec![];
        let mut state = State::new();
        loop {
            buf.clear();
            let e = xml_reader
                .read_event_into(&mut buf)
                .with_context(|| "cannot read XML event")?;
            match e {
                //start tag
                Event::Start(e) => {
                    state.open_tag(&e).with_context(|| {
                        format!(
                            "start tag `{}` at position {}",
                            String::from_utf8_lossy(e.name().as_ref()),
                            xml_reader.buffer_position()
                        )
                    })?;
                }

                //end of tag
                Event::End(e) => state.close_tag(&e).with_context(|| {
                    format!(
                        "close tag `{}` at position {}",
                        String::from_utf8_lossy(e.name().as_ref()),
                        xml_reader.buffer_position()
                    )
                })?,

                //empty tag
                Event::Empty(e) => state.empty_tag(&e).with_context(|| {
                    format!(
                        "empty tag `{}` at position {}",
                        String::from_utf8_lossy(e.name().as_ref()),
                        xml_reader.buffer_position()
                    )
                })?,

                //end of file: return the tree if we can finish
                Event::Eof => {
                    state.can_eof().with_context(|| "unexpected end of file")?;
                    return Ok(state.to_model()?);
                }

                _ => (),
            }
        }
    }
}
from_string!(BusinessProcessModelAndNotation);

impl Display for BusinessProcessModelAndNotation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

impl Infoable for BusinessProcessModelAndNotation {
    fn info(&self, f: &mut impl std::io::Write) -> Result<()> {
        todo!()
    }
}

impl Graphable for BusinessProcessModelAndNotation {
    fn to_dot(&self) -> Result<layout::topo::layout::VisualGraph> {
        todo!()
    }
}

impl TranslateActivityKey for BusinessProcessModelAndNotation {
    fn translate_using_activity_key(&mut self, to_activity_key: &mut ActivityKey) {
        todo!()
    }
}

#[cfg(any(test, feature = "testactivities"))]
impl TestActivityKey for BusinessProcessModelAndNotation {
    fn test_activity_key(&self) {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use crate::BusinessProcessModelAndNotation;
    use std::fs::{self};

    #[test]
    fn bpmn_import() {
        let fin = fs::read_to_string("testfiles/model.bpmn").unwrap();
        let log = fin.parse::<BusinessProcessModelAndNotation>().unwrap();
    }
}
