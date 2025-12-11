use anyhow::Result;
use std::fmt::Display;

use crate::{
    EventLogPython, EventLogTraceAttributes, PortableDocumentFormat, PortableNetworkGraphics,
    constants::ebi_object_type::EbiObjectType,
    ebi_objects::{
        deterministic_finite_automaton::DeterministicFiniteAutomaton,
        directly_follows_graph::DirectlyFollowsGraph, directly_follows_model::DirectlyFollowsModel,
        event_log::EventLog, event_log_csv::EventLogCsv, event_log_xes::EventLogXes,
        executions::Executions, finite_language::FiniteLanguage,
        finite_stochastic_language::FiniteStochasticLanguage, labelled_petri_net::LabelledPetriNet,
        language_of_alignments::LanguageOfAlignments, process_tree::ProcessTree,
        scalable_vector_graphics::ScalableVectorGraphics,
        stochastic_deterministic_finite_automaton::StochasticDeterministicFiniteAutomaton,
        stochastic_directly_follows_model::StochasticDirectlyFollowsModel,
        stochastic_labelled_petri_net::StochasticLabelledPetriNet,
        stochastic_language_of_alignments::StochasticLanguageOfAlignments,
        stochastic_process_tree::StochasticProcessTree,
    },
    traits::infoable::Infoable,
};

#[derive(Clone)]
pub enum EbiObject {
    LabelledPetriNet(LabelledPetriNet),
    StochasticLabelledPetriNet(StochasticLabelledPetriNet),
    FiniteStochasticLanguage(FiniteStochasticLanguage),
    StochasticDeterministicFiniteAutomaton(StochasticDeterministicFiniteAutomaton),
    EventLog(EventLog),
    EventLogCsv(EventLogCsv),
    EventLogPython(EventLogPython),
    EventLogTraceAttributes(EventLogTraceAttributes),
    EventLogXes(EventLogXes),
    FiniteLanguage(FiniteLanguage),
    DirectlyFollowsModel(DirectlyFollowsModel),
    StochasticDirectlyFollowsModel(StochasticDirectlyFollowsModel),
    LanguageOfAlignments(LanguageOfAlignments),
    StochasticLanguageOfAlignments(StochasticLanguageOfAlignments),
    DeterministicFiniteAutomaton(DeterministicFiniteAutomaton),
    ProcessTree(ProcessTree),
    StochasticProcessTree(StochasticProcessTree),
    Executions(Executions),
    DirectlyFollowsGraph(DirectlyFollowsGraph),
    ScalableVectorGraphics(ScalableVectorGraphics),
    PortableNetworkGraphics(PortableNetworkGraphics),
    PortableDocumentFormat(PortableDocumentFormat),
}

impl EbiObject {
    pub fn get_type(&self) -> EbiObjectType {
        match self {
            EbiObject::LabelledPetriNet(_) => EbiObjectType::LabelledPetriNet,
            EbiObject::StochasticLabelledPetriNet(_) => EbiObjectType::StochasticLabelledPetriNet,
            EbiObject::FiniteStochasticLanguage(_) => EbiObjectType::FiniteStochasticLanguage,
            EbiObject::StochasticDeterministicFiniteAutomaton(_) => {
                EbiObjectType::StochasticDeterministicFiniteAutomaton
            }
            EbiObject::EventLog(_) => EbiObjectType::EventLog,
            EbiObject::EventLogCsv(_) => EbiObjectType::EventLogCsv,
            EbiObject::EventLogPython(_) => EbiObjectType::EventLogPython,
            EbiObject::EventLogTraceAttributes(_) => EbiObjectType::EventLogTraceAttributes,
            EbiObject::EventLogXes(_) => EbiObjectType::EventLogXes,
            EbiObject::FiniteLanguage(_) => EbiObjectType::FiniteLanguage,
            EbiObject::DirectlyFollowsModel(_) => EbiObjectType::DirectlyFollowsModel,
            EbiObject::StochasticDirectlyFollowsModel(_) => {
                EbiObjectType::StochasticDirectlyFollowsModel
            }
            EbiObject::LanguageOfAlignments(_) => EbiObjectType::LanguageOfAlignments,
            EbiObject::StochasticLanguageOfAlignments(_) => {
                EbiObjectType::StochasticLanguageOfAlignments
            }
            EbiObject::DeterministicFiniteAutomaton(_) => {
                EbiObjectType::DeterministicFiniteAutomaton
            }
            EbiObject::ProcessTree(_) => EbiObjectType::ProcessTree,
            EbiObject::StochasticProcessTree(_) => EbiObjectType::StochasticProcessTree,
            EbiObject::Executions(_) => EbiObjectType::Executions,
            EbiObject::DirectlyFollowsGraph(_) => EbiObjectType::DirectlyFollowsGraph,
            EbiObject::ScalableVectorGraphics(_) => EbiObjectType::ScalableVectorGraphics,
            EbiObject::PortableDocumentFormat(_) => EbiObjectType::PortableDocumentFormat,
            EbiObject::PortableNetworkGraphics(_) => EbiObjectType::PortableNetworkGraphics,
        }
    }
}

impl Display for EbiObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EbiObject::LabelledPetriNet(o) => write!(f, "{}", o),
            EbiObject::StochasticLabelledPetriNet(o) => write!(f, "{}", o),
            EbiObject::FiniteStochasticLanguage(o) => write!(f, "{}", o),
            EbiObject::StochasticDeterministicFiniteAutomaton(o) => write!(f, "{}", o),
            EbiObject::EventLog(o) => write!(f, "{}", o),
            EbiObject::EventLogCsv(o) => write!(f, "{}", o),
            EbiObject::EventLogPython(o) => write!(f, "{}", o),
            EbiObject::EventLogTraceAttributes(o) => write!(f, "{}", o),
            EbiObject::EventLogXes(o) => write!(f, "{}", o),
            EbiObject::FiniteLanguage(o) => write!(f, "{}", o),
            EbiObject::DirectlyFollowsModel(o) => write!(f, "{}", o),
            EbiObject::StochasticDirectlyFollowsModel(o) => write!(f, "{}", o),
            EbiObject::LanguageOfAlignments(o) => write!(f, "{}", o),
            EbiObject::StochasticLanguageOfAlignments(o) => write!(f, "{}", o),
            EbiObject::DeterministicFiniteAutomaton(o) => write!(f, "{}", o),
            EbiObject::ProcessTree(o) => write!(f, "{}", o),
            EbiObject::StochasticProcessTree(o) => write!(f, "{}", o),
            EbiObject::Executions(o) => write!(f, "{}", o),
            EbiObject::DirectlyFollowsGraph(o) => write!(f, "{}", o),
            EbiObject::ScalableVectorGraphics(o) => write!(f, "{}", o),
            EbiObject::PortableDocumentFormat(o) => write!(f, "{}", o),
            EbiObject::PortableNetworkGraphics(o) => write!(f, "{}", o),
        }
    }
}

impl Infoable for EbiObject {
    fn info(&self, f: &mut impl std::io::Write) -> Result<()> {
        match self {
            EbiObject::LabelledPetriNet(o) => o.info(f),
            EbiObject::StochasticLabelledPetriNet(o) => o.info(f),
            EbiObject::FiniteStochasticLanguage(o) => o.info(f),
            EbiObject::StochasticDeterministicFiniteAutomaton(o) => o.info(f),
            EbiObject::EventLog(o) => o.info(f),
            EbiObject::EventLogCsv(o) => o.info(f),
            EbiObject::EventLogPython(o) => o.info(f),
            EbiObject::EventLogTraceAttributes(o) => o.info(f),
            EbiObject::EventLogXes(o) => o.info(f),
            EbiObject::FiniteLanguage(o) => o.info(f),
            EbiObject::DirectlyFollowsModel(o) => o.info(f),
            EbiObject::StochasticDirectlyFollowsModel(o) => o.info(f),
            EbiObject::LanguageOfAlignments(o) => o.info(f),
            EbiObject::StochasticLanguageOfAlignments(o) => o.info(f),
            EbiObject::DeterministicFiniteAutomaton(o) => o.info(f),
            EbiObject::ProcessTree(o) => o.info(f),
            EbiObject::StochasticProcessTree(o) => o.info(f),
            EbiObject::Executions(o) => o.info(f),
            EbiObject::DirectlyFollowsGraph(o) => o.info(f),
            EbiObject::ScalableVectorGraphics(o) => o.info(f),
            EbiObject::PortableDocumentFormat(o) => o.info(f),
            EbiObject::PortableNetworkGraphics(o) => o.info(f),
        }
    }
}
