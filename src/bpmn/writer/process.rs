use crate::bpmn::{elements::process::BPMNProcess, objects_writable::Writable};

impl Writable for BPMNProcess {
    fn write<W: std::io::Write>(
        &self,
        x: &mut quick_xml::Writer<W>,
        bpmn: &crate::BusinessProcessModelAndNotation,
    ) -> std::io::Result<()> {
        x.create_element("process")
            .with_attribute(("id", self.id.as_str()))
            .with_attribute(("isExecutable", "true"))
            .write_inner_content(|x| {
                for element in &self.elements {
                    element.write(x, bpmn)?;
                }
                Ok(())
            })?;
        Ok(())
    }
}
