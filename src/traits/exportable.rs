use anyhow::Result;

use crate::constants::ebi_object::EbiObject;

pub trait Exportable {
    fn export_from_object(object: EbiObject, f: &mut dyn std::io::Write) -> Result<()>;

    fn export(&self, f: &mut dyn std::io::Write) -> Result<()>;
}
