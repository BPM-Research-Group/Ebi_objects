use crate::constants::ebi_object::EbiObject;
use ebi_bpmn::ebi_arithmetic::anyhow::Result;

pub trait Exportable {
    /// Input: any EbiObject.
    /// Output: the object will have been converted if necessary, and written to f. May fail for all kinds of reasons.
    fn export_from_object(object: EbiObject, f: &mut dyn std::io::Write) -> Result<()>;

    /// Export the object to the writer `f`.
    fn export(&self, f: &mut dyn std::io::Write) -> Result<()>;
}
