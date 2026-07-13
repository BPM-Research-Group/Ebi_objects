use strum_macros::{Display, EnumIter, EnumString};

#[derive(PartialEq, Clone, EnumIter, Hash, Default, Debug, Display, EnumString)]
pub enum EbiObjectType {
    #[default]
    #[strum(serialize = "business process model and notation")]
    BusinessProcessModelAndNotation,
    #[strum(serialize = "stochastic business process model and notation")]
    StochasticBusinessProcessModelAndNotation,
    #[strum(serialize = "language of alignments")]
    LanguageOfAlignments,
    #[strum(serialize = "stochastic language of alignments")]
    StochasticLanguageOfAlignments,
    #[strum(serialize = "stochastic deterministic finite automaton")]
    StochasticDeterministicFiniteAutomaton,
    #[strum(serialize = "stochastic non-deterministic finite automaton")]
    StochasticNondeterministicFiniteAutomaton,
    #[strum(serialize = "deterministic finite automaton")]
    DeterministicFiniteAutomaton,
    #[strum(serialize = "directly follows model")]
    DirectlyFollowsModel,
    #[strum(serialize = "stochastic directly follows model")]
    StochasticDirectlyFollowsModel,
    #[strum(serialize = "event log")]
    EventLog,
    #[strum(serialize = "event log with event attributes")]
    EventLogEventAttributes,
    #[strum(serialize = "CSV event log")]
    EventLogCsv,
    #[strum(serialize = "OCEL event log")]
    EventLogOcel,
    #[strum(serialize = "Python event log")]
    EventLogPython,
    #[strum(serialize = "event log with trace attributes")]
    EventLogTraceAttributes,
    #[strum(serialize = "XES event log")]
    EventLogXes,
    #[strum(serialize = "finite language")]
    FiniteLanguage,
    #[strum(serialize = "finite stochastic language")]
    FiniteStochasticLanguage,
    #[strum(serialize = "finite stochastic partially ordered language")]
    FiniteStochasticPartiallyOrderedLanguage,
    #[strum(serialize = "labelled Petri net")]
    LabelledPetriNet,
    #[strum(serialize = "stochastic labelled Petri net")]
    StochasticLabelledPetriNet,
    #[strum(serialize = "process tree")]
    ProcessTree,
    #[strum(serialize = "stochastic process tree")]
    StochasticProcessTree,
    #[strum(serialize = "executions")]
    Executions,
    #[strum(serialize = "directly follows graph")]
    DirectlyFollowsGraph,
    #[strum(serialize = "scalable vector graphics")]
    ScalableVectorGraphics,
    #[strum(serialize = "portable network graphics")]
    PortableNetworkGraphics,
    #[strum(serialize = "portable document format")]
    PortableDocumentFormat,
    #[strum(serialize = "partially ordered workflow language")]
    PartiallyOrderedWorkflowLanguage,
}

impl EbiObjectType {
    pub fn get_article(&self) -> &str {
        match self {
            EbiObjectType::BusinessProcessModelAndNotation => "a",
            EbiObjectType::StochasticBusinessProcessModelAndNotation => "an",
            EbiObjectType::LabelledPetriNet => "a",
            EbiObjectType::StochasticLabelledPetriNet => "a",
            EbiObjectType::StochasticDeterministicFiniteAutomaton => "a",
            EbiObjectType::StochasticNondeterministicFiniteAutomaton => "a",
            EbiObjectType::EventLog => "an",
            EbiObjectType::EventLogCsv => "an",
            EbiObjectType::EventLogEventAttributes => "an",
            EbiObjectType::EventLogOcel => "an",
            EbiObjectType::EventLogPython => "a",
            EbiObjectType::EventLogTraceAttributes => "an",
            EbiObjectType::EventLogXes => "an",
            EbiObjectType::FiniteLanguage => "a",
            EbiObjectType::FiniteStochasticLanguage => "a",
            EbiObjectType::FiniteStochasticPartiallyOrderedLanguage => "a",
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
            EbiObjectType::PortableDocumentFormat => "a",
            EbiObjectType::PortableNetworkGraphics => "a",
            EbiObjectType::PartiallyOrderedWorkflowLanguage => "a",
        }
    }
}

impl Eq for EbiObjectType {}

impl Ord for EbiObjectType {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.to_string()
            .to_lowercase()
            .cmp(&other.to_string().to_lowercase())
    }
}

impl PartialOrd for EbiObjectType {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.to_string()
            .to_lowercase()
            .partial_cmp(&other.to_string().to_lowercase())
    }
}
