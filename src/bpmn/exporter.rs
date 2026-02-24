use crate::{
    BusinessProcessModelAndNotation, EbiObject, Exportable,
    bpmn::{
        element::BPMNElement, elements::collapsed_pool::BPMNCollapsedPool,
        objects_objectable::BPMNObject, objects_writable::Writable,
    },
};
use anyhow::{Result, anyhow};
use quick_xml::{
    Writer,
    events::{BytesDecl, BytesEnd, BytesStart, Event},
};
use std::io::Write;

impl Exportable for BusinessProcessModelAndNotation {
    fn export_from_object(object: EbiObject, f: &mut dyn Write) -> Result<()> {
        match object {
            EbiObject::BusinessProcessModelAndNotation(bpmn) => bpmn.export(f),
            _ => Err(anyhow!("Cannot export as BPMN.")),
        }
    }

    fn export(&self, f: &mut dyn Write) -> Result<()> {
        let mut x = Writer::new_with_indent(f, b'\t', 1);

        //XML declaration
        x.write_event(Event::Decl(BytesDecl::new("1.0", Some("UTF-8"), None)))?;

        //definitions
        x.write_event(Event::Start(
            BytesStart::new("definitions").with_attributes([
                ("id", self.definitions_id.as_str()),
                ("xmlns", "http://www.omg.org/spec/BPMN/20100524/MODEL"),
                ("exporter", "Ebi-bpmn"),
            ]),
        ))?;

        //collaboration
        if let Some(collaboration_id) = &self.collaboration_id {
            x.write_event(Event::Start(
                BytesStart::new("collaboration")
                    .with_attributes([("id", collaboration_id.as_str())]),
            ))?;

            //expanded pools
            self.participants.write(&mut x, self)?;

            //collapsed pools
            for element in &self.elements {
                if element.is_collapsed_pool() {
                    let mut el = x
                        .create_element("participant")
                        .with_attributes([("id", element.id())]);
                    if let BPMNElement::CollapsedPool(BPMNCollapsedPool {
                        name: Some(name), ..
                    }) = element
                    {
                        el = el.with_attribute(("name", name.as_str()));
                    }
                    el.write_empty()?;
                }
            }

            //messages
            self.message_flows.write(&mut x, self)?;

            x.write_event(Event::End(BytesEnd::new("collaboration")))?;
        }

        self.elements.write(&mut x, self)?;

        x.write_event(Event::End(BytesEnd::new("definitions")))?;

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::{BusinessProcessModelAndNotation, Exportable};
    use std::fs::{self};

    #[test]
    fn bpmn_export_import() {
        let fin = fs::read_to_string("testfiles/model-lanes.bpmn").unwrap();
        let bpmn = fin.parse::<BusinessProcessModelAndNotation>().unwrap();

        let mut f = vec![];
        bpmn.export(&mut f).unwrap();

        let fout = String::from_utf8_lossy(&f);
        let _bpmn2 = fout.parse::<BusinessProcessModelAndNotation>();

        println!("{}", String::from_utf8_lossy(&f));
    }
}
