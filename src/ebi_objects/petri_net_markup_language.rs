use super::{
    deterministic_finite_automaton::DeterministicFiniteAutomaton,
    directly_follows_graph::DirectlyFollowsGraph, directly_follows_model::DirectlyFollowsModel,
    labelled_petri_net::LabelledPetriNet, process_tree::ProcessTree,
    stochastic_deterministic_finite_automaton::StochasticDeterministicFiniteAutomaton,
    stochastic_directly_follows_model::StochasticDirectlyFollowsModel,
    stochastic_labelled_petri_net::StochasticLabelledPetriNet,
    stochastic_process_tree::StochasticProcessTree,
};
use crate::{
    Exportable, Graphable, HasActivityKey, Importable, StochasticNondeterministicFiniteAutomaton,
    TranslateActivityKey,
    constants::ebi_object::EbiObject,
    ebi_objects::{
        partially_ordered_workflow_language::PartiallyOrderedWorkflowLanguage,
        petri_net_markup_language::{
            namespace::is_in_namespace,
            parser::{can_eof, close_tag, empty_tag, open_tag},
            parser_state::ParserState,
        },
    },
    traits::importable::{ImporterParameter, ImporterParameterValues, from_string},
};
use anyhow::Context;
#[cfg(any(test, feature = "testactivities"))]
use ebi_activity_key::TestActivityKey;
use ebi_bpmn::ebi_arithmetic::anyhow::{Error, Result, anyhow};
use process_mining::core::process_models::petri_net::pnml::export_petri_net_to_pnml;
use quick_xml::{NsReader, events::Event};
use std::io::{BufRead, Write};

#[derive(Clone)]
pub struct PetriNetMarkupLanguage(pub LabelledPetriNet);

impl Importable for PetriNetMarkupLanguage {
    const FILE_FORMAT_SPECIFICATION_LATEX: &str =
        "A Petri net markup language file follows the ISO 15909-2:2011 format~\\cite{pnml}. 

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
        let mut xml_reader = NsReader::from_reader(reader);
        xml_reader.config_mut().trim_text(true);

        let mut buf = vec![];
        let mut state = ParserState::new();
        loop {
            buf.clear();
            let (namespace, xml_event) = xml_reader
                .read_resolved_event_into(&mut buf)
                .with_context(|| "Cannot read XML event.")?;
            let in_namespace = is_in_namespace(namespace);

            match (in_namespace, xml_event) {
                //start tag
                (Some(n), Event::Start(e)) => {
                    open_tag(&mut state, &e, n).with_context(|| {
                        format!(
                            "Tag `{}` at position {}.",
                            String::from_utf8_lossy(e.local_name().as_ref()),
                            xml_reader.buffer_position()
                        )
                    })?;
                }

                //end of tag
                (Some(n), Event::End(e)) => close_tag(&mut state, &e, n).with_context(|| {
                    format!(
                        "Tag `{}` at position {}.",
                        String::from_utf8_lossy(e.local_name().as_ref()),
                        xml_reader.buffer_position()
                    )
                })?,

                //empty tag
                (Some(n), Event::Empty(e)) => empty_tag(&mut state, &e, n).with_context(|| {
                    format!(
                        "Tag `{}` at position {}.",
                        String::from_utf8_lossy(e.local_name().as_ref()),
                        xml_reader.buffer_position()
                    )
                })?,

                //end of file: check whether we can finish
                (_, Event::Eof) => {
                    can_eof(&state).with_context(|| "Unexpected end of file.")?;
                    return Ok(PetriNetMarkupLanguage(state.to_lpn()?));
                }

                _ => (),
            }
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
            EbiObject::PartiallyOrderedWorkflowLanguage(powl) => {
                <PartiallyOrderedWorkflowLanguage as TryInto<LabelledPetriNet>>::try_into(powl)?
                    .export(f)
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

            EbiObject::BusinessProcessModelAndNotation(_) => {
                Err(anyhow!("Cannot export BPMN as PNML."))
            }
            EbiObject::StochasticBusinessProcessModelAndNotation(_) => {
                Err(anyhow!("Cannot export SBPMN as PNML."))
            }
            EbiObject::EventLog(_) => Err(anyhow!("Cannot export event log as PNML.")),
            EbiObject::EventLogCsv(_) => Err(anyhow!("Cannot export event log as PNML.")),
            EbiObject::EventLogEventAttributes(_) => {
                Err(anyhow!("Cannot export event log as PNML."))
            }
            EbiObject::EventLogOcel(_) => Err(anyhow!("Cannot export event log as PNML.")),
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
            EbiObject::FiniteStochasticPartiallyOrderedLanguage(_) => Err(anyhow!(
                "Cannot export finite stochastic partially ordered language as PNML."
            )),
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

// ========= importer =========

pub(crate) mod parser {
    use crate::ebi_objects::petri_net_markup_language::{
        namespace::NameSpace,
        parser_state::{ParserState, parse_attribute},
        parser_traits::{Closeable, Openable, Recognisable},
        tags::{OpenedTag, Tag},
    };
    use anyhow::{Context, Result, anyhow};
    use quick_xml::events::{BytesEnd, BytesStart};

    pub(crate) fn empty_tag(state: &mut ParserState, e: &BytesStart, n: NameSpace) -> Result<()> {
        open_tag(state, e, n)?;
        close_tag(state, &e.to_end(), n)
    }

    pub(crate) fn open_tag(state: &mut ParserState, e: &BytesStart, n: NameSpace) -> Result<()> {
        if let Some(tag) = Tag::recognise_tag(e, state, n) {
            let opened_tag =
                Tag::open_tag(tag, e, state).with_context(|| anyhow!("parsing tag `{}`", tag))?;
            state.open_tags.push(opened_tag);
        } else {
            state.open_tags.push(OpenedTag::Unknown);

            //save id'ed tags for more helpful error messages
            if let Some(id) = parse_attribute(e, "id") {
                state
                    .not_recognised_id_2_tag
                    .insert(id, String::from_utf8_lossy(e.name().as_ref()).to_string());
            }
        }

        state
            .open_tag_names
            .push(e.local_name().as_ref().to_owned());

        Ok(())
    }

    pub(crate) fn close_tag(state: &mut ParserState, e: &BytesEnd, _n: NameSpace) -> Result<()> {
        if let (Some(most_recent_open_tag_name), Some(most_recent_open_tag)) =
            (state.open_tag_names.pop(), state.open_tags.pop())
        {
            if most_recent_open_tag_name == e.local_name().as_ref() {
                //closing tag matches last remaining opening tag

                OpenedTag::close_tag(most_recent_open_tag, e, state).with_context(|| {
                    anyhow!(
                        "At the closing of tag `{}`.",
                        String::from_utf8_lossy(&most_recent_open_tag_name)
                    )
                })?;

                Ok(())
            } else {
                Err(anyhow!(
                    "attempted to close tag `{}` but `{}` was open",
                    String::from_utf8_lossy(e.local_name().as_ref()),
                    String::from_utf8_lossy(&most_recent_open_tag_name)
                ))
            }
        } else {
            Err(anyhow!(
                "attempted to close tag `{}` that was not open",
                String::from_utf8_lossy(e.local_name().as_ref())
            ))
        }
    }

    pub(crate) fn can_eof(state: &ParserState) -> Result<()> {
        if let Some(tag) = state.open_tag_names.iter().next() {
            Err(anyhow!(
                "file ended while tag `{}` was still open",
                String::from_utf8_lossy(&tag)
            ))
        } else {
            Ok(())
        }
    }
}

pub(crate) mod parser_state {
    use crate::{
        LabelledPetriNet,
        ebi_objects::petri_net_markup_language::{tag_pnml::DraftPnml, tags::OpenedTag},
    };
    use anyhow::{Result, anyhow};
    use quick_xml::events::BytesStart;
    use std::collections::{HashMap, HashSet};

    pub(crate) struct ParserState {
        pub(crate) open_tag_names: Vec<Vec<u8>>,
        pub(crate) open_tags: Vec<OpenedTag>,
        pub(crate) ids: HashSet<String>,

        pub(crate) pnmls: Vec<DraftPnml>,

        pub(crate) lpn: LabelledPetriNet,
        pub(crate) not_recognised_id_2_tag: HashMap<String, String>,
    }

    impl ParserState {
        pub(crate) fn new() -> Self {
            Self {
                open_tag_names: vec![],
                open_tags: vec![],
                pnmls: vec![],
                ids: HashSet::new(),
                not_recognised_id_2_tag: HashMap::new(),
                lpn: LabelledPetriNet::new(),
            }
        }

        pub(crate) fn read_id(&mut self, e: &BytesStart) -> Result<String> {
            if let Some(id) = parse_attribute(e, "id") {
                if !self.ids.insert(id.clone()) {
                    Err(anyhow!("two elements have the id `{}`", id))
                } else {
                    Ok(id)
                }
            } else {
                Err(anyhow!("element must have an id"))
            }
        }

        pub(crate) fn to_lpn(self) -> Result<LabelledPetriNet> {
            let ParserState { mut pnmls, .. } = self;

            if pnmls.len() == 1 {
                let draft_net = pnmls.remove(0);

                Ok(())
            } else if pnmls.is_empty() {
                Err(anyhow!("no process found"))
            } else {
                Err(anyhow!("multiple processes found"))
            }
        }
    }

    pub(crate) fn parse_attribute(e: &BytesStart, attribute_name: &str) -> Option<String> {
        if let Ok(Some(attribute)) = e.try_get_attribute(attribute_name) {
            Some(
                attribute
                    .decoded_and_normalized_value(quick_xml::XmlVersion::Implicit1_0, e.decoder())
                    .ok()?
                    .as_ref()
                    .to_owned(),
            )
        } else {
            None
        }
    }
}

pub(crate) mod namespace {
    use quick_xml::name::{Namespace, ResolveResult};
    use strum_macros::EnumIs;

    #[derive(Clone, Copy, EnumIs)]
    pub enum NameSpace {
        PNML,
    }

    pub const NAMESPACE_PNML: &[u8; 45] = b"http://www.pnml.org/version-2009/grammar/pnml";

    pub(crate) fn is_in_namespace(result: ResolveResult) -> Option<NameSpace> {
        match result {
            ResolveResult::Unbound => Some(NameSpace::PNML),
            ResolveResult::Bound(Namespace(n)) if n == NAMESPACE_PNML => Some(NameSpace::PNML),
            _ => None,
        }
    }
}

pub(crate) mod parser_traits {
    use crate::ebi_objects::petri_net_markup_language::{
        namespace::NameSpace,
        parser_state::ParserState,
        tags::{OpenedTag, Tag},
    };
    use anyhow::Result;
    use quick_xml::events::{BytesEnd, BytesStart};

    pub(crate) trait Recognisable {
        /// Given a start tag, determine whether it is recognisable
        fn recognise_tag(e: &BytesStart, state: &ParserState, n: NameSpace) -> Option<Tag>
        where
            Self: Sized;
    }

    pub(crate) trait Openable {
        /// Given a start tag that is recognisable, attempt to open it
        fn open_tag(tag: Tag, e: &BytesStart, state: &mut ParserState) -> Result<OpenedTag>
        where
            Self: Sized;
    }

    pub(crate) trait Closeable {
        /// Given an open tag, attempt to close it, and add the result to the parent in the state.
        fn close_tag(opened_tag: OpenedTag, e: &BytesEnd, state: &mut ParserState) -> Result<()>;
    }
}

pub(crate) mod tags {
    use crate::ebi_objects::petri_net_markup_language::{
        namespace::NameSpace,
        parser_state::ParserState,
        parser_traits::{Closeable, Openable, Recognisable},
        tag_pnml::TagPnml,
    };
    use anyhow::Result;
    use quick_xml::events::{BytesEnd, BytesStart};
    use strum::IntoEnumIterator;
    use strum_macros::{Display, EnumIter, EnumString};

    #[derive(Clone, Copy, EnumString, EnumIter, Display)]
    pub(crate) enum Tag {
        Pnml,
    }

    impl Recognisable for Tag {
        fn recognise_tag(e: &BytesStart, state: &ParserState, n: NameSpace) -> Option<Tag>
        where
            Self: Sized,
        {
            for tag in Tag::iter() {
                let x = match tag {
                    Tag::Pnml => TagPnml::recognise_tag(e, state, n),
                };
                if x.is_some() {
                    return x;
                }
            }
            None
        }
    }

    impl Openable for Tag {
        fn open_tag(tag: Tag, e: &BytesStart, state: &mut ParserState) -> Result<OpenedTag>
        where
            Self: Sized,
        {
            match tag {
                Tag::Pnml => TagPnml::open_tag(tag, e, state),
            }
        }
    }

    pub(crate) enum OpenedTag {
        Unknown,
        Pnml {
            id: String,
            draft_nets: Vec<DraftNet>,
        },
    }

    impl Closeable for OpenedTag {
        fn close_tag(opened_tag: OpenedTag, e: &BytesEnd, state: &mut ParserState) -> Result<()> {
            match opened_tag {
                OpenedTag::Unknown => Ok(()),
                OpenedTag::Pnml { .. } => TagPnml::close_tag(opened_tag, e, state),
            }
        }
    }
}

pub(crate) mod tag_pnml {
    use crate::ebi_objects::petri_net_markup_language::{
        namespace::NameSpace,
        parser_state::ParserState,
        parser_traits::{Closeable, Openable, Recognisable},
        tags::{OpenedTag, Tag},
    };
    use anyhow::{Result, anyhow};
    use quick_xml::events::{BytesEnd, BytesStart};

    pub struct DraftPnml {
        nets: Vec<DraftNet>,
    }

    pub struct TagPnml {}

    impl Recognisable for TagPnml {
        fn recognise_tag(e: &BytesStart, state: &ParserState, n: NameSpace) -> Option<Tag>
        where
            Self: Sized,
        {
            if n.is_pnml() {
                if state.open_tags.is_empty() {
                    if e.local_name().as_ref() == b"pnml" {
                        return Some(Tag::Pnml);
                    }
                }
                None
            } else {
                None
            }
        }
    }

    impl Openable for TagPnml {
        fn open_tag(_tag: Tag, e: &BytesStart, state: &mut ParserState) -> Result<OpenedTag>
        where
            Self: Sized,
        {
            let id = state.read_id(e)?;

            //find the stochastic declaration

            Ok(OpenedTag::Pnml {
                id,
                draft_nets: vec![],
            })
        }
    }

    impl Closeable for TagPnml {
        fn close_tag(opened_tag: OpenedTag, e: &BytesEnd, state: &mut ParserState) -> Result<()> {
            if let OpenedTag::Pnml { id, draft_nets } = opened_tag {
                let pnml = DraftPnml { nets: draft_nets };

                state.pnmls.push(pnml);
                Ok(())
            } else {
                return Err(anyhow!("Expected Pnml."));
            }
        }
    }
}