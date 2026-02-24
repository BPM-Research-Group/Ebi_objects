use crate::bpmn::{
    elements::message_intermediate_catch_event::BPMNMessageIntermediateCatchEvent,
    objects_writable::Writable,
};
use quick_xml::events::{BytesStart, BytesText, Event};

impl Writable for BPMNMessageIntermediateCatchEvent {
    fn write<W: std::io::Write>(
        &self,
        x: &mut quick_xml::Writer<W>,
        bpmn: &crate::BusinessProcessModelAndNotation,
    ) -> anyhow::Result<()> {
        x.create_element("intermediateCatchEvent")
            .with_attribute(("id", self.id.as_str()))
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
                x.write_event(Event::Empty(
                    BytesStart::new("messageEventDefinition")
                        .with_attributes([("id", self.message_marker_id.as_str())]),
                ))?;
                Ok(())
            })?;
        Ok(())
    }
}
