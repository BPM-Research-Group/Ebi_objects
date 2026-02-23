use quick_xml::events::BytesText;

use crate::{
    BusinessProcessModelAndNotation,
    bpmn::{elements::task::BPMNTask, objects_writable::Writable},
};

impl Writable for BPMNTask {
    fn write<W: std::io::Write>(
        &self,
        x: &mut quick_xml::Writer<W>,
        bpmn: &BusinessProcessModelAndNotation,
    ) -> std::io::Result<()> {
        x.create_element("task")
            .with_attribute(("id", self.id.as_str()))
            .with_attribute(("name", bpmn.activity_key.deprocess_activity(&self.activity)))
            .write_inner_content(|x| {
                for incoming_sequence_flow in &self.incoming_sequence_flows {
                    x.create_element("incoming")
                        .write_text_content(BytesText::new(
                            &bpmn.sequence_flows[*incoming_sequence_flow].id,
                        ))?;
                }
                for outgoing_sequence_flow in &self.outgoing_sequence_flows {
                    x.create_element("outgoing")
                        .write_text_content(BytesText::new(
                            &bpmn.sequence_flows[*outgoing_sequence_flow].id,
                        ))?;
                }
                Ok(())
            })?;
        Ok(())
    }
}
