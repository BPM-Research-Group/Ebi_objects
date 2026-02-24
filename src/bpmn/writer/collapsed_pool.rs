use crate::bpmn::{elements::collapsed_pool::BPMNCollapsedPool, objects_writable::Writable};

impl Writable for BPMNCollapsedPool {
    fn write<W: std::io::Write>(
        &self,
        _x: &mut quick_xml::Writer<W>,
        _bpmn: &crate::BusinessProcessModelAndNotation,
    ) -> anyhow::Result<()> {
        Ok(())
    }
}
