use std::io::{BufRead, Write};

use anyhow::{Result, anyhow};

use crate::{Exportable, Graphable, Importable, constants::ebi_object::EbiObject};

use super::{
    deterministic_finite_automaton::DeterministicFiniteAutomaton,
    directly_follows_graph::DirectlyFollowsGraph, directly_follows_model::DirectlyFollowsModel,
    labelled_petri_net::LabelledPetriNet, process_tree::ProcessTree,
    stochastic_deterministic_finite_automaton::StochasticDeterministicFiniteAutomaton,
    stochastic_directly_follows_model::StochasticDirectlyFollowsModel,
    stochastic_labelled_petri_net::StochasticLabelledPetriNet,
    stochastic_process_tree::StochasticProcessTree,
};

pub const FORMAT_SPECIFICATION: &str =
    "A Petri net markup language file follows the ISO 15909-2:2011 format~\\cite{pnml}. 
Parsing is performed by the Rust4PM crate~\\cite{DBLP:conf/bpm/KustersA24}.

    Please note that Ebi ignores any final markings.
    Instead, every deadlock is considered a final marking.

    For instance:
    \\lstinputlisting[language=xml, style=boxed]{../testfiles/a.pnml}";

#[derive(Clone)]
pub struct PetriNetMarkupLanguage {
    pub net: process_mining::PetriNet,
}

impl Importable for PetriNetMarkupLanguage {
    fn import_as_object(reader: &mut dyn BufRead) -> Result<EbiObject> {
        Ok(EbiObject::LabelledPetriNet(
            Self::import(reader)?.try_into()?,
        ))
    }

    fn import(reader: &mut dyn BufRead) -> Result<Self>
    where
        Self: Sized,
    {
        match process_mining::petri_net::import_pnml::import_pnml_reader(&mut Box::new(reader)) {
            Ok(pnml) => Ok(Self { net: pnml }),
            Err(e) => Err(anyhow!("{}", e)),
        }
    }
}

impl Exportable for PetriNetMarkupLanguage {
    fn export_from_object(object: EbiObject, f: &mut dyn Write) -> Result<()> {
        match object {
            EbiObject::DeterministicFiniteAutomaton(dfa) => {
                <DeterministicFiniteAutomaton as TryInto<PetriNetMarkupLanguage>>::try_into(dfa)?
                    .export(f)
            }
            EbiObject::DirectlyFollowsGraph(dfm) => {
                <DirectlyFollowsGraph as TryInto<PetriNetMarkupLanguage>>::try_into(dfm)?.export(f)
            }
            EbiObject::DirectlyFollowsModel(dfm)=> {
                <DirectlyFollowsModel as TryInto<PetriNetMarkupLanguage>>::try_into(dfm)?.export(f)
            }
            EbiObject::StochasticDirectlyFollowsModel(dfm) => {
                <StochasticDirectlyFollowsModel as TryInto<PetriNetMarkupLanguage>>::try_into(dfm)?.export(f)
            }
            EbiObject::LabelledPetriNet(lpn) => {
                <LabelledPetriNet as TryInto<PetriNetMarkupLanguage>>::try_into(lpn)?.export(f)
            }
            EbiObject::ProcessTree(tree) => {
                <ProcessTree as TryInto<PetriNetMarkupLanguage>>::try_into(tree)?.export(f)
            }
            EbiObject::StochasticProcessTree(tree) => {
                <StochasticProcessTree as TryInto<PetriNetMarkupLanguage>>::try_into(tree)?.export(f)
            }
            EbiObject::StochasticDeterministicFiniteAutomaton(sdfa) => {
                <StochasticDeterministicFiniteAutomaton as TryInto<PetriNetMarkupLanguage>>::try_into(
                    sdfa,
                )
                ?.export(f)
            }
            EbiObject::StochasticLabelledPetriNet(slpn) => {
                <StochasticLabelledPetriNet as TryInto<PetriNetMarkupLanguage>>::try_into(slpn)?
                    .export(f)
            }


           EbiObject::EventLog(_) => {
                Err(anyhow!("Cannot export event log as PNML."))
            }
            EbiObject::Executions(_) => {
                Err(anyhow!("Cannot export executions as PNML."))
            }
            EbiObject::FiniteLanguage(_) => {
                Err(anyhow!("Cannot export finite language as PNML."))
            }
            EbiObject::FiniteStochasticLanguage(_) => {
                Err(anyhow!("Cannot export finite stochastic language as PNML."))
            }
            EbiObject::LanguageOfAlignments(_) => {
                Err(anyhow!("Cannot export language of alignments as PNML."))
            }
            EbiObject::StochasticLanguageOfAlignments(_) => Err(anyhow!(
                "Cannot export stochastic language of alignments as PNML."
            )),
            EbiObject::ScalableVectorGraphics(_) => Err(anyhow!(
                "Cannot export scalable vector graphics as PNML."
            )),
        }
    }

    fn export(&self, f: &mut dyn std::io::Write) -> Result<()> {
        process_mining::petri_net::export_pnml::export_petri_net_to_pnml(&self.net, f)?;
        Ok(())
    }
}

impl Graphable for PetriNetMarkupLanguage {
    fn to_dot(&self) -> Result<layout::topo::layout::VisualGraph> {
        TryInto::<LabelledPetriNet>::try_into(self.clone())?.to_dot()
    }
}
