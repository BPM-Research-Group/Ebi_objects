#[cfg(any(test, feature = "testactivities"))]
use crate::activity_key::has_activity_key::TestActivityKey;
use crate::{
    Activity, ActivityKey, EbiObject, Graphable, Importable, Infoable, TranslateActivityKey,
    traits::importable::{ImporterParameter, ImporterParameterValues, from_string},
};
use anyhow::{Context, Result, anyhow};
use ebi_derive::ActivityKey;
use itertools::Itertools;
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

type BPMNProcess = Vec<BPMNElement>;

#[derive(Clone, Debug)]
enum BPMNElement {
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
    fn parse(e: &BytesStart) -> Option<Self> {
        match e.name().as_ref() {
            b"bpmn:startEvent" => Some(BPMNElementType::StartEvent),
            b"bpmn:endEvent" => Some(BPMNElementType::EndEvent),
            b"bpmn:task" => Some(BPMNElementType::Task),
            b"bpmn:exclusiveGateway" => Some(BPMNElementType::ExclusiveGateway),
            b"bpmn:inclusiveGateway" => Some(BPMNElementType::InclusiveGateway),
            _ => None,
        }
    }

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

    fn to_element(self, e: &BytesStart, state: &mut State) -> Result<BPMNElement> {
        Ok(match self {
            BPMNElementType::StartEvent => BPMNElement::StartEvent,
            BPMNElementType::EndEvent => BPMNElement::EndEvent,
            BPMNElementType::Task => {
                if let Some(label) = BPMNElementType::parse_attribute(e, "name") {
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

enum BPMNFlowType {
    SequenceFlow,
    MessageFlow,
}

impl BPMNFlowType {
    fn from_str(s: &[u8]) -> Option<Self> {
        match s {
            b"bpmn:sequenceFlow" => Some(BPMNFlowType::SequenceFlow),
            b"bpmn:messageFlow" => Some(BPMNFlowType::MessageFlow),
            _ => None,
        }
    }
}

struct State {
    activity_key: ActivityKey,
    open_tags: Vec<Vec<u8>>,
    open_elements: Vec<BPMNElementType>,
    in_definitions: bool,
    in_process: bool,
    closed_processes: usize,
    id_2_element: HashMap<Vec<u8>, BPMNElement>,
}

impl State {
    fn new() -> Self {
        Self {
            activity_key: ActivityKey::new(),
            open_tags: vec![],
            open_elements: vec![],
            in_definitions: false,
            in_process: false,
            closed_processes: 0,
            id_2_element: HashMap::new(),
        }
    }

    fn empty_tag(&mut self, e: BytesStart) -> Result<()> {
        self.open_tag(e.clone())?;
        self.close_tag(e.to_end())
    }

    fn open_tag(&mut self, e: BytesStart) -> Result<()> {
        match (
            e.name().as_ref(),
            BPMNElementType::parse(&e),
            BPMNFlowType::from_str(e.name().as_ref()),
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
            (_, Some(typee), _) => {
                self.open_element(typee, &e)
                    .with_context(|| anyhow!("parsing tag `{:?}`", e.name().as_ref()))?;
            }
            _ => {}
        };

        self.open_tags.push(e.name().as_ref().to_owned());

        println!("{}", debug(&self.open_tags));

        Ok(())
    }

    fn close_tag(&mut self, e: BytesEnd) -> Result<()> {
        if let Some(last_tag) = self.open_tags.pop() {
            if last_tag == e.name().as_ref() {
                match e.name().as_ref() {
                    b"bpmn:process" => {
                        self.closed_processes += 1;
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
        } else if self.closed_processes < 1 {
            return Err(anyhow!(
                "no `bpmn:process` tag found nested in a `bpmn:definitions` tag"
            ));
        } else {
            Ok(())
        }
    }

    fn open_element(&mut self, element_type: BPMNElementType, e: &BytesStart) -> Result<()> {
        println!("\topen element {:?}", element_type);

        //create the element
        let element = element_type.to_element(e, self)?;

        //read the id
        if let Ok(Some(attribute)) = e.try_get_attribute("id") {
            let id = attribute.value;
            match self.id_2_element.entry(id.to_vec()) {
                Entry::Occupied(_) => {
                    return Err(anyhow!(
                        "two nodes have the id `{}`",
                        String::from_utf8_lossy(&id)
                    ));
                }
                Entry::Vacant(vacant_entry) => {
                    vacant_entry.insert(element);
                }
            }
        } else {
            return Err(anyhow!("element must have an id"));
        }

        Ok(())
    }

    fn to_model(self) -> Result<BusinessProcessModelAndNotation> {
        todo!()
    }
}

fn debug(inn: &Vec<Vec<u8>>) -> String {
    inn.iter()
        .map(|s| String::from_utf8_lossy(s).to_string())
        .join(", ")
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
            let e = xml_reader.read_event_into(&mut buf);
            // log::debug!("xml reads {:?}", e);
            match e {
                //start tag
                Ok(Event::Start(e)) => {
                    state
                        .open_tag(e)
                        .with_context(|| format!("at position {}", xml_reader.buffer_position()))?;
                }

                //end of tag
                Ok(Event::End(e)) => state
                    .close_tag(e)
                    .with_context(|| format!("at position {}", xml_reader.buffer_position()))?,

                //empty tag
                Ok(Event::Empty(e)) => state
                    .empty_tag(e)
                    .with_context(|| format!("at position {}", xml_reader.buffer_position()))?,

                //end of file: return the tree if we can finish
                Ok(Event::Eof) => {
                    state.can_eof()?;
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
        // let fin = fs::read_to_string("testfiles/model.bpmn").unwrap();
        // let log = fin.parse::<BusinessProcessModelAndNotation>().unwrap();
    }
}
