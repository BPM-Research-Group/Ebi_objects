use quick_xml::events::{BytesEnd, BytesStart, Event};

use crate::bpmn::{elements::process::BPMNProcess, objects_writable::Writable};

impl Writable for BPMNProcess {
    fn write<W: std::io::Write>(
        &self,
        x: &mut quick_xml::Writer<W>,
        bpmn: &crate::BusinessProcessModelAndNotation,
    ) -> anyhow::Result<()> {
        x.write_event(Event::Start(BytesStart::new("process").with_attributes([
            ("id", self.id.as_str()),
            ("isExecutable", "true"),
        ])))?;

        self.elements.write(x, bpmn)?;

        x.write_event(Event::End(BytesEnd::new("process")))?;
        Ok(())
    }
}
