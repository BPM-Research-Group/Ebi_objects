use crate::bpmn::{
    elements::message_start_event::BPMNMessageStartEvent, objects_writable::Writable,
};
use quick_xml::events::{BytesStart, BytesText, Event};

impl Writable for BPMNMessageStartEvent {
    fn write<W: std::io::Write>(
        &self,
        x: &mut quick_xml::Writer<W>,
        bpmn: &crate::BusinessProcessModelAndNotation,
    ) -> anyhow::Result<()> {
        x.create_element("startEvent")
            .with_attributes([("id", self.id.as_str())])
            .write_inner_content(|x| {
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
