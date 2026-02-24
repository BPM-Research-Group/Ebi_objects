use crate::bpmn::{
    objects_objectable::BPMNObject, objects_writable::Writable, sequence_flow::BPMNSequenceFlow,
};
use anyhow::anyhow;
use quick_xml::events::{BytesStart, Event};

impl Writable for BPMNSequenceFlow {
    fn write<W: std::io::Write>(
        &self,
        x: &mut quick_xml::Writer<W>,
        bpmn: &crate::BusinessProcessModelAndNotation,
    ) -> anyhow::Result<()> {
        let source_id = bpmn
            .index_2_element(self.source_index)
            .ok_or_else(|| anyhow!("source not found"))?
            .id();
        let target_id = bpmn
            .index_2_element(self.target_index)
            .ok_or_else(|| anyhow!("target not found"))?
            .id();

        x.write_event(Event::Empty(
            BytesStart::new("sequenceFlow").with_attributes([
                ("id", self.id.as_str()),
                ("sourceRef", source_id),
                ("targetRef", target_id),
            ]),
        ))?;
        Ok(())
    }
}
