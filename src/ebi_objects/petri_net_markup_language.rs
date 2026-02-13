use super::{
    deterministic_finite_automaton::DeterministicFiniteAutomaton,
    directly_follows_graph::DirectlyFollowsGraph, directly_follows_model::DirectlyFollowsModel,
    labelled_petri_net::LabelledPetriNet, process_tree::ProcessTree,
    stochastic_deterministic_finite_automaton::StochasticDeterministicFiniteAutomaton,
    stochastic_directly_follows_model::StochasticDirectlyFollowsModel,
    stochastic_labelled_petri_net::StochasticLabelledPetriNet,
    stochastic_process_tree::StochasticProcessTree,
};
#[cfg(any(test, feature = "testactivities"))]
use crate::activity_key::has_activity_key::TestActivityKey;
use crate::{
    Exportable, Graphable, HasActivityKey, Importable, StochasticNondeterministicFiniteAutomaton,
    TranslateActivityKey,
    constants::ebi_object::EbiObject,
    traits::importable::{ImporterParameter, ImporterParameterValues, from_string},
};
use anyhow::{Result, anyhow};
use process_mining::core::process_models::petri_net::pnml::{
    export_petri_net_to_pnml, import_pnml_reader,
};
use std::io::{BufRead, Write};

#[derive(Clone)]
pub struct PetriNetMarkupLanguage(pub LabelledPetriNet);

impl Importable for PetriNetMarkupLanguage {
    const FILE_FORMAT_SPECIFICATION_LATEX: &str =
        "A Petri net markup language file follows the ISO 15909-2:2011 format~\\cite{pnml}. 
Parsing is performed by the Rust4PM crate~\\cite{DBLP:conf/bpm/KustersA24}.

    Please note that Ebi ignores any final markings.
    Instead, every deadlock is considered a final marking.

    For instance:
    \\lstinputlisting[language=xml, style=boxed]{../testfiles/a.pnml}";

    const IMPORTER_PARAMETERS: &[ImporterParameter] = &[];

    fn import_as_object(
        reader: &mut dyn BufRead,
        parameter_values: &ImporterParameterValues,
    ) -> Result<EbiObject> {
        Ok(EbiObject::LabelledPetriNet(
            Self::import(reader, parameter_values)?.try_into()?,
        ))
    }

    fn import(reader: &mut dyn BufRead, _: &ImporterParameterValues) -> Result<Self>
    where
        Self: Sized,
    {
        match import_pnml_reader(&mut Box::new(reader)) {
            Ok(pnml) => Ok(Self(pnml.try_into()?)),
            Err(e) => Err(anyhow!("{}", e)),
        }
    }
}
from_string!(PetriNetMarkupLanguage);

impl Exportable for PetriNetMarkupLanguage {
    fn export_from_object(object: EbiObject, f: &mut dyn Write) -> Result<()> {
        match object {
            EbiObject::DeterministicFiniteAutomaton(dfa) => {
                <DeterministicFiniteAutomaton as TryInto<LabelledPetriNet>>::try_into(dfa)?
                    .export(f)
            }
            EbiObject::DirectlyFollowsGraph(dfm) => Ok(<DirectlyFollowsGraph as TryInto<
                LabelledPetriNet,
            >>::try_into(dfm)?
            .export(f)?),
            EbiObject::DirectlyFollowsModel(dfm) => {
                <DirectlyFollowsModel as TryInto<LabelledPetriNet>>::try_into(dfm)?.export(f)
            }
            EbiObject::StochasticDirectlyFollowsModel(dfm) => {
                <StochasticDirectlyFollowsModel as TryInto<LabelledPetriNet>>::try_into(dfm)?
                    .export(f)
            }
            EbiObject::LabelledPetriNet(lpn) => {
                <LabelledPetriNet as TryInto<LabelledPetriNet>>::try_into(lpn)?.export(f)
            }
            EbiObject::ProcessTree(tree) => {
                <ProcessTree as TryInto<LabelledPetriNet>>::try_into(tree)?.export(f)
            }
            EbiObject::StochasticProcessTree(tree) => {
                <StochasticProcessTree as TryInto<LabelledPetriNet>>::try_into(tree)?.export(f)
            }
            EbiObject::StochasticDeterministicFiniteAutomaton(sdfa) => {
                <StochasticDeterministicFiniteAutomaton as TryInto<LabelledPetriNet>>::try_into(
                    sdfa,
                )?
                .export(f)
            }
            EbiObject::StochasticNondeterministicFiniteAutomaton(sdfa) => {
                <StochasticNondeterministicFiniteAutomaton as TryInto<LabelledPetriNet>>::try_into(
                    sdfa,
                )?
                .export(f)
            }
            EbiObject::StochasticLabelledPetriNet(slpn) => {
                <StochasticLabelledPetriNet as TryInto<LabelledPetriNet>>::try_into(slpn)?.export(f)
            }

            EbiObject::EventLog(_) => Err(anyhow!("Cannot export event log as PNML.")),
            EbiObject::EventLogCsv(_) => Err(anyhow!("Cannot export event log as PNML.")),
            EbiObject::EventLogPython(_) => Err(anyhow!("Cannot export event log as PNML.")),
            EbiObject::EventLogTraceAttributes(_) => {
                Err(anyhow!("Cannot export event log as PNML."))
            }
            EbiObject::EventLogXes(_) => Err(anyhow!("Cannot export event log as PNML.")),
            EbiObject::Executions(_) => Err(anyhow!("Cannot export executions as PNML.")),
            EbiObject::FiniteLanguage(_) => Err(anyhow!("Cannot export finite language as PNML.")),
            EbiObject::FiniteStochasticLanguage(_) => {
                Err(anyhow!("Cannot export finite stochastic language as PNML."))
            }
            EbiObject::LanguageOfAlignments(_) => {
                Err(anyhow!("Cannot export language of alignments as PNML."))
            }
            EbiObject::StochasticLanguageOfAlignments(_) => Err(anyhow!(
                "Cannot export stochastic language of alignments as PNML."
            )),
            EbiObject::ScalableVectorGraphics(_) => {
                Err(anyhow!("Cannot export scalable vector graphics as PNML."))
            }
            EbiObject::PortableDocumentFormat(_) => {
                Err(anyhow!("Cannot export portable document format as PNML."))
            }
            EbiObject::PortableNetworkGraphics(_) => {
                Err(anyhow!("Cannot export portable network graphics as PNML."))
            }
        }
    }

    fn export(&self, f: &mut dyn std::io::Write) -> Result<()> {
        let pnml = (&(self.0)).try_into()?;
        export_petri_net_to_pnml(&pnml, f)?;
        Ok(())
    }
}

impl Graphable for PetriNetMarkupLanguage {
    fn to_dot(&self) -> Result<layout::topo::layout::VisualGraph> {
        TryInto::<LabelledPetriNet>::try_into(self.clone())?.to_dot()
    }
}

impl HasActivityKey for PetriNetMarkupLanguage {
    fn activity_key(&self) -> &crate::ActivityKey {
        &self.0.activity_key
    }

    fn activity_key_mut(&mut self) -> &mut crate::ActivityKey {
        &mut self.0.activity_key
    }
}

impl TranslateActivityKey for PetriNetMarkupLanguage {
    fn translate_using_activity_key(&mut self, to_activity_key: &mut crate::ActivityKey) {
        self.0.translate_using_activity_key(to_activity_key);
    }
}

#[cfg(any(test, feature = "testactivities"))]
impl TestActivityKey for PetriNetMarkupLanguage {
    fn test_activity_key(&self) {
        self.0.test_activity_key();
    }
}
