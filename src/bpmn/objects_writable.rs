use crate::{BusinessProcessModelAndNotation, bpmn::element::BPMNElement};
use quick_xml::Writer;
use std::io::{Result, Write};

pub(crate) trait Writable {
    fn write<W: Write>(
        &self,
        x: &mut Writer<W>,
        bpmn: &BusinessProcessModelAndNotation,
    ) -> Result<()>;
}

impl Writable for Vec<BPMNElement> {
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
