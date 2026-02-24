use crate::bpmn::{
    elements::expanded_sub_process::BPMNExpandedSubProcess, objects_writable::Writable,
};
use quick_xml::events::{BytesEnd, BytesStart, BytesText, Event};

impl Writable for BPMNExpandedSubProcess {
    fn write<W: std::io::Write>(
        &self,
        x: &mut quick_xml::Writer<W>,
        bpmn: &crate::BusinessProcessModelAndNotation,
    ) -> anyhow::Result<()> {
        let mut attributes = vec![("id", self.id.as_str())];
        if let Some(name) = &self.name {
            attributes.push(("name", name.as_str()));
        }

        x.write_event(Event::Start(
            BytesStart::new("subProcess").with_attributes(attributes),
        ))?;

        //incoming
        for incoming_sequence_flow in &self.incoming_sequence_flows {
            x.create_element("incoming")
                .write_text_content(BytesText::new(
                    &bpmn.sequence_flows[*incoming_sequence_flow].id,
                ))?;
        }

        //outgoing
        for outgoing_sequence_flow in &self.outgoing_sequence_flows {
            x.create_element("outgoing")
                .write_text_content(BytesText::new(
                    &bpmn.sequence_flows[*outgoing_sequence_flow].id,
                ))?;
        }

        //recursive elements
        self.elements.write(x, bpmn)?;

        x.write_event(Event::End(BytesEnd::new("subProcess")))?;
        Ok(())
    }
}
