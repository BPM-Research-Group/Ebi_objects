use std::fmt::Display;

use strum_macros::EnumIter;

#[derive(PartialEq, Clone, EnumIter, Hash, Default, Debug)]
pub enum EbiObjectType {
    #[default]
    LanguageOfAlignments,
    StochasticLanguageOfAlignments,
    StochasticDeterministicFiniteAutomaton,
    DeterministicFiniteAutomaton,
    DirectlyFollowsModel,
    StochasticDirectlyFollowsModel,
    EventLog,
    FiniteLanguage,
    FiniteStochasticLanguage,
    LabelledPetriNet,
    StochasticLabelledPetriNet,
    ProcessTree,
    StochasticProcessTree,
    Executions,
    DirectlyFollowsGraph,
    ScalableVectorGraphics,
}

impl EbiObjectType {
    pub fn get_article(&self) -> &str {
        match self {
            EbiObjectType::LabelledPetriNet => "a",
            EbiObjectType::StochasticLabelledPetriNet => "a",
            EbiObjectType::FiniteStochasticLanguage => "a",
            EbiObjectType::StochasticDeterministicFiniteAutomaton => "a",
            EbiObjectType::EventLog => "an",
            EbiObjectType::FiniteLanguage => "a",
            EbiObjectType::DirectlyFollowsModel => "a",
            EbiObjectType::StochasticDirectlyFollowsModel => "a",
            EbiObjectType::LanguageOfAlignments => "",
            EbiObjectType::StochasticLanguageOfAlignments => "an",
            EbiObjectType::DeterministicFiniteAutomaton => "a",
            EbiObjectType::ProcessTree => "a",
            EbiObjectType::StochasticProcessTree => "a",
            EbiObjectType::Executions => "",
            EbiObjectType::DirectlyFollowsGraph => "a",
            EbiObjectType::ScalableVectorGraphics => "a",
        }
    }
}

impl Eq for EbiObjectType {}

impl Display for EbiObjectType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                EbiObjectType::LabelledPetriNet => "labelled Petri net",
                EbiObjectType::StochasticLabelledPetriNet => "stochastic labelled Petri net",
                EbiObjectType::FiniteStochasticLanguage => "finite stochastic language",
                EbiObjectType::StochasticDeterministicFiniteAutomaton =>
                    "stochastic deterministic finite automaton",
                EbiObjectType::EventLog => "event log",
                EbiObjectType::FiniteLanguage => "finite language",
                EbiObjectType::DirectlyFollowsModel => "directly follows model",
                EbiObjectType::StochasticDirectlyFollowsModel =>
                    "stochastic directly follows model",
                EbiObjectType::LanguageOfAlignments => "alignments",
                EbiObjectType::StochasticLanguageOfAlignments =>
                    "stochastic language of alignments",
                EbiObjectType::DeterministicFiniteAutomaton => "deterministic finite automaton",
                EbiObjectType::ProcessTree => "process tree",
                EbiObjectType::StochasticProcessTree => "stochastic process tree",
                EbiObjectType::Executions => "executions",
                EbiObjectType::DirectlyFollowsGraph => "directly follows graph",
                EbiObjectType::ScalableVectorGraphics => "scalable vector graphics",
            }
        )
    }
}
