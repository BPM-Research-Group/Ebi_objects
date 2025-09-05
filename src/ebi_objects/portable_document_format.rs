use anyhow::{Result, anyhow};
use std::fmt::Display;
use svg2pdf::{ConversionOptions, PageOptions};

use crate::{
    Exportable,
    constants::ebi_object::EbiObject,
    ebi_objects::scalable_vector_graphics::{ScalableVectorGraphics, ToSVG},
};

pub const FORMAT_SPECIFICATION: &str = "Ebi does not support importing PDF files.";

pub struct PortableDocumentFormat(Vec<u8>);

impl Display for PortableDocumentFormat {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        "pdf".fmt(f)
    }
}

impl From<Vec<u8>> for PortableDocumentFormat {
    fn from(value: Vec<u8>) -> Self {
        Self(value)
    }
}

impl Exportable for PortableDocumentFormat {
    fn export_from_object(object: EbiObject, f: &mut dyn std::io::Write) -> Result<()> {
        match object {
            EbiObject::DeterministicFiniteAutomaton(object) => object.to_pdf()?.export(f),
            EbiObject::DirectlyFollowsModel(object) => object.to_pdf()?.export(f),
            EbiObject::StochasticDirectlyFollowsModel(object) => object.to_pdf()?.export(f),
            EbiObject::EventLog(_) => Err(anyhow!("cannot export event log as PDF")),
            EbiObject::Executions(_) => Err(anyhow!("cannot export executions as PDF")),
            EbiObject::FiniteLanguage(_) => Err(anyhow!("cannot export finite language as PDF")),
            EbiObject::FiniteStochasticLanguage(_) => {
                Err(anyhow!("cannot export finite stochastic as PDF"))
            }
            EbiObject::LabelledPetriNet(object) => object.to_pdf()?.export(f),
            EbiObject::LanguageOfAlignments(_) => {
                Err(anyhow!("cannot export language of alignments as PDF"))
            }
            EbiObject::ProcessTree(object) => object.to_pdf()?.export(f),
            EbiObject::StochasticDeterministicFiniteAutomaton(object) => object.to_pdf()?.export(f),
            EbiObject::StochasticLabelledPetriNet(object) => object.to_pdf()?.export(f),
            EbiObject::StochasticLanguageOfAlignments(_) => Err(anyhow!(
                "cannot export stochastic language of alignments as PDF"
            )),
            EbiObject::StochasticProcessTree(object) => object.to_pdf()?.export(f),
            EbiObject::DirectlyFollowsGraph(object) => object.to_pdf()?.export(f),
            EbiObject::ScalableVectorGraphics(object) => object.to_pdf()?.export(f),
        }
    }

    fn export(&self, f: &mut dyn std::io::Write) -> anyhow::Result<()> {
        Ok(f.write_all(&self.0)?)
    }
}

pub trait ToPDF {
    fn to_pdf(&self) -> Result<PortableDocumentFormat>;
}

impl<T> ToPDF for T
where
    T: ToSVG,
{
    fn to_pdf(&self) -> Result<PortableDocumentFormat> {
        self.to_svg()?.to_pdf()
    }
}

impl ToPDF for ScalableVectorGraphics {
    fn to_pdf(&self) -> Result<PortableDocumentFormat> {
        let mut options = svg2pdf::usvg::Options::default();
        options.fontdb_mut().load_system_fonts();
        let tree = svg2pdf::usvg::Tree::from_str(&self.0, &options)?;
        match svg2pdf::to_pdf(&tree, ConversionOptions::default(), PageOptions::default()) {
            Ok(x) => Ok(x.into()),
            Err(err) => Err(anyhow!(err)),
        }
    }
}
