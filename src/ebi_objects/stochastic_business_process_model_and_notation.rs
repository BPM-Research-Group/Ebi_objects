use crate::{
    EbiObject, Exportable, Graphable, Importable, Infoable,
    traits::importable::{ImporterParameter, ImporterParameterValues},
};
use anyhow::{Result, anyhow};
use ebi_bpmn::stochastic_business_process_model_and_notation::StochasticBusinessProcessModelAndNotation;
use layout::topo::layout::VisualGraph;
use std::io::Write;

impl Importable for StochasticBusinessProcessModelAndNotation {
    const IMPORTER_PARAMETERS: &[ImporterParameter] = &[];

    const FILE_FORMAT_SPECIFICATION_LATEX: &str =
        "A stochastic Business Process Model and Notation (BPMN) model follows the OMG 2.0.2 standard~\\cite{omg2011bpmn}.
        In addition, certain sequence flows must have <weight constant=\"..\"> tags in the namespace https://www.ebitools.org/sbpmn/20260305.
    Currently, a sub-set of elements is supported.";

    fn import_as_object(
        reader: &mut dyn std::io::BufRead,
        parameter_values: &ImporterParameterValues,
    ) -> Result<EbiObject> {
        Ok(EbiObject::StochasticBusinessProcessModelAndNotation(
            Self::import(reader, parameter_values)?,
        ))
    }

    fn import(
        reader: &mut dyn std::io::BufRead,
        _parameter_values: &ImporterParameterValues,
    ) -> Result<Self>
    where
        Self: Sized,
    {
        StochasticBusinessProcessModelAndNotation::import_from_reader(reader)
    }
}

impl Infoable for StochasticBusinessProcessModelAndNotation {
    fn info(&self, f: &mut impl Write) -> Result<()> {
        self.bpmn.info(f)
    }
}

impl Graphable for StochasticBusinessProcessModelAndNotation {
    fn to_dot(&self) -> Result<VisualGraph> {
        self.bpmn.to_dot()
    }
}

impl Exportable for StochasticBusinessProcessModelAndNotation {
    fn export_from_object(object: EbiObject, f: &mut dyn std::io::Write) -> Result<()> {
        match object {
            EbiObject::StochasticBusinessProcessModelAndNotation(bpmn) => bpmn.export(f),
            EbiObject::BusinessProcessModelAndNotation(bpmn) => {
                StochasticBusinessProcessModelAndNotation::try_from(bpmn)?.export(f)
            }
            _ => Err(anyhow!(
                "Cannot export {} {} as SBPMN.",
                object.get_type().get_article(),
                object.get_type()
            )),
        }
    }

    fn export(&self, f: &mut dyn std::io::Write) -> Result<()> {
        self.export_to_writer(f)
    }
}
