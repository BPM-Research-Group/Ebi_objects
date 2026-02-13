#[cfg(any(test, feature = "testactivities"))]
use crate::activity_key::has_activity_key::TestActivityKey;
use crate::{
    EbiObject, Exportable, Infoable, ScalableVectorGraphics,
    ebi_objects::scalable_vector_graphics::ToSVG,
};
use anyhow::{Result, anyhow};
use resvg::usvg::{self};
use std::fmt::Display;

#[derive(Clone)]
pub struct PortableNetworkGraphics(pub Vec<u8>);

pub const FORMAT_SPECIFICATION: &str = "Ebi does not support importing PNG files.";

impl Display for PortableNetworkGraphics {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        "png".fmt(f)
    }
}

impl From<Vec<u8>> for PortableNetworkGraphics {
    fn from(value: Vec<u8>) -> Self {
        Self(value)
    }
}

impl Exportable for PortableNetworkGraphics {
    fn export_from_object(object: EbiObject, f: &mut dyn std::io::Write) -> Result<()> {
        match object {
            EbiObject::DeterministicFiniteAutomaton(object) => object.to_png()?.export(f),
            EbiObject::DirectlyFollowsModel(object) => object.to_png()?.export(f),
            EbiObject::StochasticDirectlyFollowsModel(object) => object.to_png()?.export(f),
            EbiObject::EventLog(_) => Err(anyhow!("cannot export event log as PNG")),
            EbiObject::EventLogCsv(_) => Err(anyhow!("cannot export event log as PNG")),
            EbiObject::EventLogPython(_) => Err(anyhow!("cannot export event log as PNG")),
            EbiObject::EventLogTraceAttributes(_) => Err(anyhow!("cannot export event log as PNG")),
            EbiObject::EventLogXes(_) => Err(anyhow!("cannot export event log as PNG")),
            EbiObject::Executions(_) => Err(anyhow!("cannot export executions as PNG")),
            EbiObject::FiniteLanguage(_) => Err(anyhow!("cannot export finite language as PNG")),
            EbiObject::FiniteStochasticLanguage(_) => {
                Err(anyhow!("cannot export finite stochastic as PNG"))
            }
            EbiObject::LabelledPetriNet(object) => object.to_png()?.export(f),
            EbiObject::LanguageOfAlignments(_) => {
                Err(anyhow!("cannot export language of alignments as PNG"))
            }
            EbiObject::ProcessTree(object) => object.to_png()?.export(f),
            EbiObject::StochasticDeterministicFiniteAutomaton(object) => object.to_png()?.export(f),
            EbiObject::StochasticNondeterministicFiniteAutomaton(object) => {
                object.to_png()?.export(f)
            }
            EbiObject::StochasticLabelledPetriNet(object) => object.to_png()?.export(f),
            EbiObject::StochasticLanguageOfAlignments(_) => Err(anyhow!(
                "cannot export stochastic language of alignments as PNG"
            )),
            EbiObject::StochasticProcessTree(object) => object.to_png()?.export(f),
            EbiObject::DirectlyFollowsGraph(object) => object.to_png()?.export(f),
            EbiObject::ScalableVectorGraphics(object) => object.to_png()?.export(f),
            EbiObject::PortableDocumentFormat(_) => {
                Err(anyhow!("cannot export portable document format as PNG"))
            }
            EbiObject::PortableNetworkGraphics(object) => object.export(f),
        }
    }

    fn export(&self, f: &mut dyn std::io::Write) -> anyhow::Result<()> {
        Ok(f.write_all(&self.0)?)
    }
}

impl Infoable for PortableNetworkGraphics {
    fn info(&self, f: &mut impl std::io::Write) -> Result<()> {
        Ok(writeln!(f, "PNG object")?)
    }
}

pub trait ToPNG {
    fn to_png(&self) -> Result<PortableNetworkGraphics>;
}

impl<T> ToPNG for T
where
    T: ToSVG,
{
    fn to_png(&self) -> Result<PortableNetworkGraphics> {
        self.to_svg()?.to_png()
    }
}

impl ToPNG for ScalableVectorGraphics {
    fn to_png(&self) -> Result<PortableNetworkGraphics> {
        let tree = {
            let mut opt = usvg::Options::default();
            opt.fontdb_mut().load_system_fonts();
            usvg::Tree::from_str(&self.0, &opt)?
        };

        let size = tree.size().to_int_size();
        let mut pixmap = tiny_skia::Pixmap::new(size.width(), size.height())
            .ok_or_else(|| anyhow!("could not create pixmap"))?;
        resvg::render(&tree, tiny_skia::Transform::default(), &mut pixmap.as_mut());

        Ok(PortableNetworkGraphics(pixmap.encode_png()?))
    }
}

#[cfg(any(test, feature = "testactivities"))]
impl TestActivityKey for PortableNetworkGraphics {
    fn test_activity_key(&self) {
        //no activities are stored
    }
}
