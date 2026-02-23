use crate::{
    BusinessProcessModelAndNotation, EbiObject, Importable,
    bpmn::parser::{
        parser::{can_eof, close_tag, empty_tag, is_in_namespace, open_tag},
        parser_state::ParserState,
    },
    traits::importable::{ImporterParameter, ImporterParameterValues, from_string},
};
use anyhow::{Context, Result};
use quick_xml::{
    NsReader,
    events::{BytesStart, Event},
};
use std::io::BufRead;

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
        reader: &mut dyn BufRead,
        _parameter_values: &crate::traits::importable::ImporterParameterValues,
    ) -> Result<Self>
    where
        Self: Sized,
    {
        let mut xml_reader = NsReader::from_reader(reader);
        xml_reader.config_mut().trim_text(true);

        let mut buf = vec![];
        let mut state = ParserState::new();
        loop {
            buf.clear();
            let (namespace, xml_event) = xml_reader
                .read_resolved_event_into(&mut buf)
                .with_context(|| "cannot read XML event")?;
            let in_namespace = is_in_namespace(namespace);
            match (in_namespace, xml_event) {
                //start tag
                (true, Event::Start(e)) => {
                    open_tag(&mut state, &e).with_context(|| {
                        format!(
                            "start tag `{}` at position {}",
                            String::from_utf8_lossy(e.local_name().as_ref()),
                            xml_reader.buffer_position()
                        )
                    })?;
                }

                //end of tag
                (true, Event::End(e)) => close_tag(&mut state, &e).with_context(|| {
                    format!(
                        "close tag `{}` at position {}",
                        String::from_utf8_lossy(e.local_name().as_ref()),
                        xml_reader.buffer_position()
                    )
                })?,

                //empty tag
                (true, Event::Empty(e)) => empty_tag(&mut state, &e).with_context(|| {
                    format!(
                        "empty tag `{}` at position {}",
                        String::from_utf8_lossy(e.local_name().as_ref()),
                        xml_reader.buffer_position()
                    )
                })?,

                //end of file: check whether we can finish
                (_, Event::Eof) => {
                    can_eof(&state).with_context(|| "unexpected end of file")?;
                    return Ok(state.to_model()?);
                }

                _ => (),
            }
        }
    }
}
from_string!(BusinessProcessModelAndNotation);

pub(crate) fn parse_attribute(e: &BytesStart, attribute_name: &str) -> Option<String> {
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

#[cfg(test)]
mod tests {
    use crate::BusinessProcessModelAndNotation;
    use std::fs::{self};

    #[test]
    fn bpmn_import() {
        let fin = fs::read_to_string("testfiles/model.bpmn").unwrap();
        let bpmn = fin.parse::<BusinessProcessModelAndNotation>().unwrap();

        assert_eq!(bpmn.sequence_flows.len(), 10);
    }

    #[test]
    #[should_panic]
    fn bpmn_pool_import() {
        let fin = fs::read_to_string("testfiles/model-pool.bpmn").unwrap();
        let bpmn = fin.parse::<BusinessProcessModelAndNotation>().unwrap();

        assert_eq!(bpmn.elements.len(), 2);
        assert_eq!(bpmn.sequence_flows.len(), 8);
        assert_eq!(bpmn.message_flows.len(), 2);
    }

    #[test]
    #[should_panic]
    fn bpmn_message_invalid() {
        let fin = fs::read_to_string("testfiles/invalid-message.bpmn").unwrap();
        fin.parse::<BusinessProcessModelAndNotation>().unwrap();
    }

    #[test]
    fn bpmn_lanes_import() {
        let fin = fs::read_to_string("testfiles/model-lanes.bpmn").unwrap();
        let bpmn = fin.parse::<BusinessProcessModelAndNotation>().unwrap();

        assert_eq!(bpmn.elements.len(), 2);
        assert_eq!(bpmn.message_flows.len(), 1);
    }
}
