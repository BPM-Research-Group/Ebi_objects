use quick_xml::events::BytesText;

use crate::bpmn::{
    elements::collapsed_sub_process::BPMNCollapsedSubProcess, objects_writable::Writable,
};

impl Writable for BPMNCollapsedSubProcess {
    fn write<W: std::io::Write>(
        &self,
        x: &mut quick_xml::Writer<W>,
        bpmn: &crate::BusinessProcessModelAndNotation,
    ) -> anyhow::Result<()> {
        let mut el = x
            .create_element("subProcess")
            .with_attribute(("id", self.id.as_str()));

        if let Some(name) = &self.name {
            el = el.with_attributes([("name", name.as_str())]);
        }

        el.write_inner_content(|x| {
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
