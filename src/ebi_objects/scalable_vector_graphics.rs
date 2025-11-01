use std::fmt::Display;

use anyhow::{Result, anyhow};
use layout::{backends::svg::SVGWriter, topo::layout::VisualGraph};

use crate::{EbiObject, Exportable, Graphable, Infoable};

pub const FORMAT_SPECIFICATION: &str = "Ebi does not support importing of SVG files.";

#[derive(Clone)]
pub struct ScalableVectorGraphics(pub String);

impl ScalableVectorGraphics {
    pub fn empty() -> Self {
        Self("<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?><svg width=\"352.5\" height=\"141\" viewBox=\"0 0 352.5 141\" xmlns=\"http://www.w3.org/2000/svg\"></svg>".to_string())
    }
}

impl Display for ScalableVectorGraphics {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl Infoable for ScalableVectorGraphics {
    fn info(&self, f: &mut impl std::io::Write) -> Result<()> {
        Ok(write!(f, "SVG object")?)
    }
}

impl Exportable for ScalableVectorGraphics {
    fn export_from_object(object: EbiObject, f: &mut dyn std::io::Write) -> Result<()> {
        match object {
            EbiObject::DeterministicFiniteAutomaton(object) => object.to_svg()?.export(f),
            EbiObject::DirectlyFollowsModel(object) => object.to_svg()?.export(f),
            EbiObject::StochasticDirectlyFollowsModel(object) => object.to_svg()?.export(f),
            EbiObject::EventLog(_) => Err(anyhow!("cannot export event log as SVG")),
            EbiObject::EventLogCsv(_) => Err(anyhow!("cannot export event log as SVG")),
            EbiObject::EventLogTraceAttributes(_) => Err(anyhow!("cannot export event log as SVG")),
            EbiObject::EventLogXes(_) => Err(anyhow!("cannot export event log as SVG")),
            EbiObject::Executions(_) => Err(anyhow!("cannot export executions as SVG")),
            EbiObject::FiniteLanguage(_) => Err(anyhow!("cannot export finite language as SVG")),
            EbiObject::FiniteStochasticLanguage(_) => {
                Err(anyhow!("cannot export finite stochastic as SVG"))
            }
            EbiObject::LabelledPetriNet(object) => object.to_svg()?.export(f),
            EbiObject::LanguageOfAlignments(_) => {
                Err(anyhow!("cannot export language of alignments as SVG"))
            }
            EbiObject::ProcessTree(object) => object.to_svg()?.export(f),
            EbiObject::StochasticDeterministicFiniteAutomaton(object) => object.to_svg()?.export(f),
            EbiObject::StochasticLabelledPetriNet(object) => object.to_svg()?.export(f),
            EbiObject::StochasticLanguageOfAlignments(_) => Err(anyhow!(
                "cannot export stochastic language of alignments as SVG"
            )),
            EbiObject::StochasticProcessTree(object) => object.to_svg()?.export(f),
            EbiObject::DirectlyFollowsGraph(object) => object.to_svg()?.export(f),
            EbiObject::ScalableVectorGraphics(object) => object.export(f),
        }
    }

    fn export(&self, f: &mut dyn std::io::Write) -> Result<()> {
        Ok(write!(f, "{}", self)?)
    }
}

impl From<String> for ScalableVectorGraphics {
    fn from(value: String) -> Self {
        Self(value)
    }
}

pub trait ToSVG {
    fn to_svg(&self) -> Result<ScalableVectorGraphics>;
}

impl<T> ToSVG for T
where
    T: Graphable,
{
    fn to_svg(&self) -> Result<ScalableVectorGraphics> {
        let mut svg = SVGWriter::new();
        let mut graph = self.to_dot()?;
        Ok(if graph.num_nodes() == 0 {
            ScalableVectorGraphics::empty()
        } else {
            graph.do_it(false, false, false, &mut svg);
            ScalableVectorGraphics(svg.finalize())
        })
    }
}

pub trait ToSVGMut {
    fn to_svg_mut(&mut self) -> Result<ScalableVectorGraphics>;
}

impl ToSVGMut for VisualGraph {
    fn to_svg_mut(&mut self) -> Result<ScalableVectorGraphics> {
        let mut svg = SVGWriter::new();
        self.do_it(false, false, false, &mut svg);
        Ok(svg.finalize().into())
    }
}
