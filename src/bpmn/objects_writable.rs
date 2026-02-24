use crate::{
    BusinessProcessModelAndNotation,
    bpmn::{
        element::BPMNElement, elements::participant::BPMNParticipant,
        message_flow::BPMNMessageFlow, sequence_flow::BPMNSequenceFlow,
    },
};
use anyhow::Result;
use quick_xml::Writer;
use std::io::Write;

pub(crate) trait Writable {
    fn write<W: Write>(
        &self,
        x: &mut Writer<W>,
        bpmn: &BusinessProcessModelAndNotation,
    ) -> Result<()>;
}

macro_rules! vec_writable {
    ($t:ty) => {
        impl Writable for $t {
            fn write<W: Write>(
                &self,
                x: &mut Writer<W>,
                bpmn: &BusinessProcessModelAndNotation,
            ) -> Result<()> {
                for element in self {
                    element.write(x, bpmn)?;
                }
                Ok(())
            }
        }
    };
}

vec_writable!(Vec<BPMNElement>);
vec_writable!(Vec<BPMNMessageFlow>);
vec_writable!(Vec<BPMNParticipant>);
vec_writable!(Vec<&BPMNSequenceFlow>);
