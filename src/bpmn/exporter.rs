use crate::{
    BusinessProcessModelAndNotation, EbiObject, Exportable, bpmn::objects_writable::Writable,
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
    fn bpmn_export() {
        let fin = fs::read_to_string("testfiles/model.bpmn").unwrap();
        let bpmn = fin.parse::<BusinessProcessModelAndNotation>().unwrap();

        let mut f = vec![];
        bpmn.export(&mut f).unwrap();

        println!("{}", String::from_utf8_lossy(&f));
    }
}
