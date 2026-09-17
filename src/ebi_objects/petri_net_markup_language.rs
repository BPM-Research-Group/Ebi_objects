use super::labelled_petri_net::LabelledPetriNet;
use crate::{
    Exportable, Graphable, HasActivityKey, Importable, TranslateActivityKey,
    constants::ebi_object::EbiObject,
    ebi_objects::petri_net_markup_language::{
        namespace::is_in_namespace,
        parser::{can_eof, close_tag, empty_tag, open_tag, text_tag},
        parser_state::ParserState,
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

#[derive(Clone, Debug)]
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
                            e.local_name().as_ref(),
                            xml_reader.buffer_position()
                        )
                    })?;
                }

                //text
                (Some(n), Event::Text(e)) => text_tag(&mut state, &e, n).with_context(|| {
                    format!("Text at position {}.", xml_reader.buffer_position())
                })?,

                //end of tag
                (Some(n), Event::End(e)) => close_tag(&mut state, &e, n).with_context(|| {
                    format!(
                        "Tag `{}` at position {}.",
                        e.local_name().as_ref(),
                        xml_reader.buffer_position()
                    )
                })?,

                //empty tag
                (Some(n), Event::Empty(e)) => empty_tag(&mut state, &e, n).with_context(|| {
                    format!(
                        "Tag `{}` at position {}.",
                        e.local_name().as_ref(),
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
                PetriNetMarkupLanguage::from(dfa).export(f)
            }
            EbiObject::DirectlyFollowsGraph(dfm) => PetriNetMarkupLanguage::from(dfm).export(f),
            EbiObject::DirectlyFollowsModel(dfm) => PetriNetMarkupLanguage::from(dfm).export(f),
            EbiObject::StochasticDirectlyFollowsModel(dfm) => {
                PetriNetMarkupLanguage::from(dfm).export(f)
            }
            EbiObject::LabelledPetriNet(lpn) => PetriNetMarkupLanguage::from(lpn).export(f),
            EbiObject::ProcessTree(tree) => PetriNetMarkupLanguage::from(tree).export(f),
            EbiObject::StochasticProcessTree(tree) => PetriNetMarkupLanguage::from(tree).export(f),
            EbiObject::PartiallyOrderedWorkflowLanguage(powl) => {
                PetriNetMarkupLanguage::from(powl).export(f)
            }
            EbiObject::StochasticDeterministicFiniteAutomaton(sdfa) => {
                PetriNetMarkupLanguage::from(sdfa).export(f)
            }
            EbiObject::StochasticNondeterministicFiniteAutomaton(sdfa) => {
                PetriNetMarkupLanguage::from(sdfa).export(f)
            }
            EbiObject::StochasticLabelledPetriNet(slpn) => {
                PetriNetMarkupLanguage::from(slpn).export(f)
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
    use quick_xml::events::{BytesEnd, BytesStart, BytesText};

    pub(crate) fn empty_tag(state: &mut ParserState, e: &BytesStart, n: NameSpace) -> Result<()> {
        open_tag(state, e, n)?;
        close_tag(state, &e.to_end(), n)
    }

    pub(crate) fn text_tag(state: &mut ParserState, e: &BytesText, n: NameSpace) -> Result<()> {
        if n.is_pnml() {
            match state.open_tags.iter_mut().last() {
                Some(OpenedTag::Text { text }) => {
                    if text.is_some() {
                        return Err(anyhow!("Cannot set text twice."));
                    }

                    *text = Some(
                        e.xml_content(quick_xml::XmlVersion::Implicit1_0)
                            .to_string(),
                    );
                }
                _ => (),
            }
        }
        Ok(())
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
                    .insert(id, e.name().as_ref().to_string());
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
                    anyhow!("At the closing of tag `{}`.", &most_recent_open_tag_name)
                })?;

                Ok(())
            } else {
                Err(anyhow!(
                    "Attempted to close tag `{}` but `{}` was open.",
                    e.local_name().as_ref(),
                    &most_recent_open_tag_name
                ))
            }
        } else {
            Err(anyhow!(
                "Attempted to close tag `{}` that was not open.",
                e.local_name().as_ref()
            ))
        }
    }

    pub(crate) fn can_eof(state: &ParserState) -> Result<()> {
        if let Some(tag) = state.open_tag_names.iter().next() {
            Err(anyhow!("file ended while tag `{}` was still open", &tag))
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
        pub(crate) open_tag_names: Vec<String>,
        pub(crate) open_tags: Vec<OpenedTag>,
        pub(crate) ids: HashSet<String>,
        pub(crate) pnmls: Vec<DraftPnml>,
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
            }
        }

        pub(crate) fn read_id(&mut self, e: &BytesStart) -> Result<String> {
            if let Some(id) = parse_attribute(e, "id") {
                if !self.ids.insert(id.clone()) {
                    Err(anyhow!("Two elements have the id `{}`.", id))
                } else {
                    Ok(id)
                }
            } else {
                Err(anyhow!("Element must have an id."))
            }
        }

        pub(crate) fn to_lpn(self) -> Result<LabelledPetriNet> {
            let ParserState { mut pnmls, .. } = self;

            if pnmls.len() == 1 {
                let mut pnml = pnmls.remove(0);

                if pnml.nets.len() == 1 {
                    let mut net = pnml.nets.remove(0);

                    if net.pages.len() == 1 {
                        let page = net.pages.remove(0);
                        Ok(page.lpn)
                    } else if net.pages.is_empty() {
                        Err(anyhow!("No page found in net `{}`.", net.id))
                    } else {
                        Err(anyhow!("Ebi does not support multiple pages."))
                    }
                } else if pnml.nets.is_empty() {
                    Err(anyhow!("No net found."))
                } else {
                    Err(anyhow!("Ebi does not support multiple nets."))
                }
            } else if pnmls.is_empty() {
                Err(anyhow!("No pnml found."))
            } else {
                Err(anyhow!("Ebi does not support multiple pnml tags."))
            }
        }
    }

    pub(crate) fn parse_attribute(e: &BytesStart, attribute_name: &str) -> Option<String> {
        if let Ok(Some(attribute)) = e.try_get_attribute(attribute_name) {
            Some(
                attribute
                    .normalized_value(quick_xml::XmlVersion::Implicit1_0)
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

    pub const NAMESPACE_PNML: &str = "http://www.pnml.org/version-2009/grammar/pnml";

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
    use crate::{
        LabelledPetriNet,
        ebi_objects::petri_net_markup_language::{
            namespace::NameSpace,
            parser_state::ParserState,
            parser_traits::{Closeable, Openable, Recognisable},
            tag_arc::{DraftArc, TagArc},
            tag_initial_marking::TagInitialMarking,
            tag_inscription::TagInscription,
            tag_name::TagName,
            tag_net::{Net, TagNet},
            tag_page::{Page, TagPage},
            tag_place::TagPlace,
            tag_pnml::TagPnml,
            tag_text::TagText,
            tag_tool_specific::TagToolSpecific,
            tag_transition::TagTransition,
        },
    };
    use anyhow::Result;
    use fnv::FnvBuildHasher;
    use quick_xml::events::{BytesEnd, BytesStart};
    use std::collections::HashMap;
    use strum::IntoEnumIterator;
    use strum_macros::{Display, EnumIter, EnumString};

    #[derive(Clone, Copy, EnumString, EnumIter, Display)]
    pub(crate) enum Tag {
        Pnml,
        Net,
        Page,
        Place,
        InitialMarking,
        Text,
        Transition,
        Name,
        ToolSpecific,
        Arc,
        Inscription,
    }

    impl Recognisable for Tag {
        fn recognise_tag(e: &BytesStart, state: &ParserState, n: NameSpace) -> Option<Tag>
        where
            Self: Sized,
        {
            for tag in Tag::iter() {
                let x = match tag {
                    Tag::Pnml => TagPnml::recognise_tag(e, state, n),
                    Tag::Net => TagNet::recognise_tag(e, state, n),
                    Tag::Page => TagPage::recognise_tag(e, state, n),
                    Tag::Place => TagPlace::recognise_tag(e, state, n),
                    Tag::InitialMarking => TagInitialMarking::recognise_tag(e, state, n),
                    Tag::Text => TagText::recognise_tag(e, state, n),
                    Tag::Transition => TagTransition::recognise_tag(e, state, n),
                    Tag::Name => TagName::recognise_tag(e, state, n),
                    Tag::ToolSpecific => TagToolSpecific::recognise_tag(e, state, n),
                    Tag::Arc => TagArc::recognise_tag(e, state, n),
                    Tag::Inscription => TagInscription::recognise_tag(e, state, n),
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
                Tag::Net => TagNet::open_tag(tag, e, state),
                Tag::Page => TagPage::open_tag(tag, e, state),
                Tag::Place => TagPlace::open_tag(tag, e, state),
                Tag::InitialMarking => TagInitialMarking::open_tag(tag, e, state),
                Tag::Text => TagText::open_tag(tag, e, state),
                Tag::Transition => TagTransition::open_tag(tag, e, state),
                Tag::Name => TagName::open_tag(tag, e, state),
                Tag::ToolSpecific => TagToolSpecific::open_tag(tag, e, state),
                Tag::Arc => TagArc::open_tag(tag, e, state),
                Tag::Inscription => TagInscription::open_tag(tag, e, state),
            }
        }
    }

    #[derive(Debug)]
    pub(crate) enum OpenedTag {
        Unknown,
        Pnml {
            nets: Vec<Net>,
        },
        Net {
            id: String,
            typee: Option<String>,
            pages: Vec<Page>,
        },
        Page {
            id: String,
            lpn: LabelledPetriNet,
            place_id_2_rank: HashMap<String, usize, FnvBuildHasher>,
            transition_id_2_rank: HashMap<String, usize, FnvBuildHasher>,
            draft_arcs: Vec<DraftArc>,
        },
        Place {
            id: String,
            initial_tokens: u64,
        },
        InitialMarking {
            tokens: Option<u64>,
        },
        Text {
            text: Option<String>,
        },
        Transition {
            id: String,
            invisible_tag: bool,
            label: Option<String>,
        },
        Name {
            name: Option<String>,
        },
        ToolSpecific {},
        Arc {
            id: String,
            source_id: String,
            target_id: String,
            cardinality: Option<u64>,
        },
        Inscription {
            inscription: Option<String>,
        },
    }

    impl Closeable for OpenedTag {
        fn close_tag(opened_tag: OpenedTag, e: &BytesEnd, state: &mut ParserState) -> Result<()> {
            match opened_tag {
                OpenedTag::Unknown => Ok(()),
                OpenedTag::Pnml { .. } => TagPnml::close_tag(opened_tag, e, state),
                OpenedTag::Net { .. } => TagNet::close_tag(opened_tag, e, state),
                OpenedTag::Page { .. } => TagPage::close_tag(opened_tag, e, state),
                OpenedTag::Place { .. } => TagPlace::close_tag(opened_tag, e, state),
                OpenedTag::InitialMarking { .. } => {
                    TagInitialMarking::close_tag(opened_tag, e, state)
                }
                OpenedTag::Text { .. } => TagText::close_tag(opened_tag, e, state),
                OpenedTag::Transition { .. } => TagTransition::close_tag(opened_tag, e, state),
                OpenedTag::Name { .. } => TagName::close_tag(opened_tag, e, state),
                OpenedTag::ToolSpecific {} => TagToolSpecific::close_tag(opened_tag, e, state),
                OpenedTag::Arc { .. } => TagArc::close_tag(opened_tag, e, state),
                OpenedTag::Inscription { .. } => TagInscription::close_tag(opened_tag, e, state),
            }
        }
    }
}

pub(crate) mod tag_pnml {
    use crate::ebi_objects::petri_net_markup_language::{
        namespace::NameSpace,
        parser_state::ParserState,
        parser_traits::{Closeable, Openable, Recognisable},
        tag_net::Net,
        tags::{OpenedTag, Tag},
    };
    use anyhow::{Result, anyhow};
    use quick_xml::events::{BytesEnd, BytesStart};

    pub struct DraftPnml {
        pub(crate) nets: Vec<Net>,
    }

    pub struct TagPnml {}

    impl Recognisable for TagPnml {
        fn recognise_tag(e: &BytesStart, state: &ParserState, n: NameSpace) -> Option<Tag>
        where
            Self: Sized,
        {
            if n.is_pnml() {
                if state.open_tags.is_empty() {
                    if e.local_name().as_ref() == "pnml" {
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
        fn open_tag(_tag: Tag, _e: &BytesStart, _state: &mut ParserState) -> Result<OpenedTag>
        where
            Self: Sized,
        {
            Ok(OpenedTag::Pnml { nets: vec![] })
        }
    }

    impl Closeable for TagPnml {
        fn close_tag(opened_tag: OpenedTag, _e: &BytesEnd, state: &mut ParserState) -> Result<()> {
            if let OpenedTag::Pnml { nets } = opened_tag {
                let pnml = DraftPnml { nets };

                state.pnmls.push(pnml);
                Ok(())
            } else {
                return Err(anyhow!("Expected Pnml."));
            }
        }
    }
}

pub(crate) mod tag_net {
    use crate::ebi_objects::petri_net_markup_language::{
        namespace::NameSpace,
        parser_state::{ParserState, parse_attribute},
        parser_traits::{Closeable, Openable, Recognisable},
        tag_page::Page,
        tags::{OpenedTag, Tag},
    };
    use anyhow::{Result, anyhow};
    use quick_xml::events::{BytesEnd, BytesStart};

    #[derive(Debug)]
    pub struct Net {
        pub id: String,
        #[allow(dead_code)]
        pub typee: Option<String>,
        pub pages: Vec<Page>,
    }

    pub(crate) struct TagNet {}

    impl Recognisable for TagNet {
        fn recognise_tag(e: &BytesStart, state: &ParserState, n: NameSpace) -> Option<Tag>
        where
            Self: Sized,
        {
            if n.is_pnml() {
                match state.open_tags.iter().last() {
                    Some(OpenedTag::Pnml { .. }) => {
                        if e.local_name().as_ref() == "net" {
                            return Some(Tag::Net);
                        }
                    }
                    _ => (),
                }
                None
            } else {
                None
            }
        }
    }

    impl Openable for TagNet {
        fn open_tag(_tag: Tag, e: &BytesStart, state: &mut ParserState) -> Result<OpenedTag>
        where
            Self: Sized,
        {
            let id = state.read_id(e)?;
            let typee = parse_attribute(e, "type");

            Ok(OpenedTag::Net {
                id,
                typee,
                pages: vec![],
            })
        }
    }

    impl Closeable for TagNet {
        fn close_tag(opened_tag: OpenedTag, _e: &BytesEnd, state: &mut ParserState) -> Result<()> {
            let index = state.open_tags.len() - 1;
            if let Some(OpenedTag::Pnml {
                nets: draft_nets, ..
            }) = state.open_tags.get_mut(index)
            {
                if let OpenedTag::Net { id, typee, pages } = opened_tag {
                    draft_nets.push(Net { id, typee, pages });
                }
                Ok(())
            } else {
                return Err(anyhow!("Expected pnml."));
            }
        }
    }
}

mod tag_page {
    use std::collections::HashMap;

    use crate::{
        LabelledPetriNet,
        ebi_objects::petri_net_markup_language::{
            namespace::NameSpace,
            parser_state::ParserState,
            parser_traits::{Closeable, Openable, Recognisable},
            tag_arc::DraftArc,
            tags::{OpenedTag, Tag},
        },
    };
    use anyhow::{Result, anyhow};
    use fnv::FnvBuildHasher;
    use quick_xml::events::{BytesEnd, BytesStart};

    #[derive(Debug)]
    pub struct Page {
        #[allow(dead_code)]
        pub id: String,

        //design decision: do not support paging, reference transitions, etc.
        //consequently, we directly parse to an LPN struct from here
        pub lpn: LabelledPetriNet,
    }

    pub(crate) struct TagPage {}

    impl Recognisable for TagPage {
        fn recognise_tag(e: &BytesStart, state: &ParserState, n: NameSpace) -> Option<Tag>
        where
            Self: Sized,
        {
            if n.is_pnml() {
                match state.open_tags.iter().last() {
                    Some(OpenedTag::Net { .. }) => {
                        if e.local_name().as_ref() == "page" {
                            return Some(Tag::Page);
                        }
                    }
                    _ => (),
                }
                None
            } else {
                None
            }
        }
    }

    impl Openable for TagPage {
        fn open_tag(_tag: Tag, e: &BytesStart, state: &mut ParserState) -> Result<OpenedTag>
        where
            Self: Sized,
        {
            let id = state.read_id(e)?;

            Ok(OpenedTag::Page {
                id,
                lpn: LabelledPetriNet::new(),
                place_id_2_rank: HashMap::<_, _, FnvBuildHasher>::default(),
                transition_id_2_rank: HashMap::<_, _, FnvBuildHasher>::default(),
                draft_arcs: vec![],
            })
        }
    }

    impl Closeable for TagPage {
        fn close_tag(opened_tag: OpenedTag, _e: &BytesEnd, state: &mut ParserState) -> Result<()> {
            let index = state.open_tags.len() - 1;
            if let Some(OpenedTag::Net { pages, .. }) = state.open_tags.get_mut(index) {
                if let OpenedTag::Page {
                    id,
                    mut lpn,
                    draft_arcs,
                    place_id_2_rank,
                    transition_id_2_rank,
                } = opened_tag
                {
                    //process arcs
                    for draft_arc in draft_arcs {
                        let DraftArc {
                            id: arc_id,
                            source_id,
                            target_id,
                            cardinality,
                        } = draft_arc;

                        let source_place_rank = place_id_2_rank.get(&source_id);
                        let source_transition_rank = transition_id_2_rank.get(&source_id);
                        let target_place_rank = place_id_2_rank.get(&target_id);
                        let target_transition_rank = transition_id_2_rank.get(&target_id);

                        match (
                            source_place_rank,
                            source_transition_rank,
                            target_place_rank,
                            target_transition_rank,
                        ) {
                            (Some(source), None, None, Some(target)) => {
                                //place to transition
                                lpn.add_place_transition_arc(*source, *target, cardinality)?;
                            }
                            (None, Some(source), Some(target), None) => {
                                lpn.add_transition_place_arc(*source, *target, cardinality)?;
                            }
                            (None, None, _, _) => {
                                return Err(anyhow!(
                                    "Page `{}` contains an arc `{}` which has a source id `{}` that was not found.",
                                    id,
                                    arc_id,
                                    source_id
                                ));
                            }
                            (_, _, None, None) => {
                                return Err(anyhow!(
                                    "Page `{}` contains an arc `{}` which has a target id `{}` that was not found.",
                                    id,
                                    arc_id,
                                    target_id
                                ));
                            }
                            _ => {
                                return Err(anyhow!(
                                    "Page `{}` contains an invalid arc `{}`.",
                                    id,
                                    arc_id
                                ));
                            }
                        };
                    }

                    //push lpn up to page
                    pages.push(Page { id, lpn });
                }
                Ok(())
            } else {
                return Err(anyhow!("Expected a net."));
            }
        }
    }
}

mod tag_place {
    use crate::ebi_objects::petri_net_markup_language::{
        namespace::NameSpace,
        parser_state::ParserState,
        parser_traits::{Closeable, Openable, Recognisable},
        tags::{OpenedTag, Tag},
    };
    use anyhow::{Result, anyhow};
    use quick_xml::events::{BytesEnd, BytesStart};

    pub(crate) struct TagPlace;

    impl Recognisable for TagPlace {
        fn recognise_tag(e: &BytesStart, state: &ParserState, n: NameSpace) -> Option<Tag>
        where
            Self: Sized,
        {
            if n.is_pnml() {
                match state.open_tags.iter().last() {
                    Some(OpenedTag::Page { .. }) => {
                        if e.local_name().as_ref() == "place" {
                            return Some(Tag::Place);
                        }
                    }
                    _ => (),
                }
                None
            } else {
                None
            }
        }
    }

    impl Openable for TagPlace {
        fn open_tag(_tag: Tag, e: &BytesStart, state: &mut ParserState) -> Result<OpenedTag>
        where
            Self: Sized,
        {
            let id = state.read_id(e)?;

            Ok(OpenedTag::Place {
                id,
                initial_tokens: 0,
            })
        }
    }

    impl Closeable for TagPlace {
        fn close_tag(opened_tag: OpenedTag, _e: &BytesEnd, state: &mut ParserState) -> Result<()> {
            if let Some(OpenedTag::Page {
                lpn,
                place_id_2_rank,
                ..
            }) = state.open_tags.iter_mut().last()
            {
                if let OpenedTag::Place {
                    id, initial_tokens, ..
                } = opened_tag
                {
                    let place_rank = lpn.add_place();
                    place_id_2_rank.insert(id, place_rank);

                    if initial_tokens > 0 {
                        lpn.initial_marking
                            .as_mut()
                            .unwrap()
                            .increase(place_rank, initial_tokens)?;
                    }
                }
                Ok(())
            } else {
                return Err(anyhow!("Expected a page."));
            }
        }
    }
}

pub(crate) mod tag_initial_marking {
    use crate::ebi_objects::petri_net_markup_language::{
        namespace::NameSpace,
        parser_state::ParserState,
        parser_traits::{Closeable, Openable, Recognisable},
        tags::{OpenedTag, Tag},
    };
    use anyhow::{Result, anyhow};
    use quick_xml::events::{BytesEnd, BytesStart};

    pub(crate) struct TagInitialMarking {}

    impl Recognisable for TagInitialMarking {
        fn recognise_tag(e: &BytesStart, state: &ParserState, n: NameSpace) -> Option<Tag>
        where
            Self: Sized,
        {
            if n.is_pnml() {
                match state.open_tags.iter().last() {
                    Some(OpenedTag::Place { .. }) => {
                        if e.local_name().as_ref() == "initialMarking" {
                            return Some(Tag::InitialMarking);
                        }
                    }
                    _ => (),
                }
                None
            } else {
                None
            }
        }
    }

    impl Openable for TagInitialMarking {
        fn open_tag(_tag: Tag, _e: &BytesStart, _state: &mut ParserState) -> Result<OpenedTag>
        where
            Self: Sized,
        {
            Ok(OpenedTag::InitialMarking { tokens: None })
        }
    }

    impl Closeable for TagInitialMarking {
        fn close_tag(opened_tag: OpenedTag, _e: &BytesEnd, state: &mut ParserState) -> Result<()> {
            let index = state.open_tags.len() - 1;
            if let Some(OpenedTag::Place { initial_tokens, .. }) = state.open_tags.get_mut(index) {
                if let OpenedTag::InitialMarking { tokens, .. } = opened_tag {
                    if let Some(tokens) = tokens {
                        *initial_tokens += tokens;
                        Ok(())
                    } else {
                        Err(anyhow!("Found an initial marking tag without tokens."))
                    }
                } else {
                    Err(anyhow!("Expected an initial marking."))
                }
            } else {
                Err(anyhow!("Expected a place."))
            }
        }
    }
}

pub(crate) mod tag_text {
    use crate::ebi_objects::petri_net_markup_language::{
        namespace::NameSpace,
        parser_state::ParserState,
        parser_traits::{Closeable, Openable, Recognisable},
        tags::{OpenedTag, Tag},
    };
    use anyhow::{Context, Result, anyhow};
    use quick_xml::events::{BytesEnd, BytesStart};

    pub(crate) struct TagText {}

    impl Recognisable for TagText {
        fn recognise_tag(e: &BytesStart, state: &ParserState, n: NameSpace) -> Option<Tag>
        where
            Self: Sized,
        {
            if n.is_pnml() {
                match state.open_tags.iter().last() {
                    Some(
                        OpenedTag::InitialMarking { .. }
                        | OpenedTag::Name { .. }
                        | OpenedTag::Inscription { .. },
                    ) => {
                        if e.local_name().as_ref() == "text" {
                            return Some(Tag::Text);
                        }
                    }
                    _ => (),
                }
                None
            } else {
                None
            }
        }
    }

    impl Openable for TagText {
        fn open_tag(_tag: Tag, _e: &BytesStart, _state: &mut ParserState) -> Result<OpenedTag>
        where
            Self: Sized,
        {
            Ok(OpenedTag::Text { text: None })
        }
    }

    impl Closeable for TagText {
        fn close_tag(opened_tag: OpenedTag, _e: &BytesEnd, state: &mut ParserState) -> Result<()> {
            let index = state.open_tags.len() - 1;

            if let OpenedTag::Text { text } = opened_tag {
                match state.open_tags.get_mut(index) {
                    //text in initial marking
                    Some(OpenedTag::InitialMarking { tokens, .. }) => {
                        if tokens.is_some() {
                            Err(anyhow!("Found an initial marking with two text tags."))
                        } else {
                            if let Some(text) = text {
                                let t = text.parse::<u64>().with_context(|| {
                                    anyhow!("Cannot parse `{}` as a number of tokens.", text)
                                })?;
                                *tokens = Some(t);
                                Ok(())
                            } else {
                                Err(anyhow!("Found a text tag without text."))
                            }
                        }
                    }

                    //name
                    Some(OpenedTag::Name { name }) => {
                        if name.is_some() {
                            Err(anyhow!("Found a name with two text tags."))
                        } else {
                            if let Some(text) = text {
                                *name = Some(text);
                                Ok(())
                            } else {
                                Err(anyhow!("Found a text tag without text."))
                            }
                        }
                    }

                    //inscription
                    Some(OpenedTag::Inscription { inscription }) => {
                        if inscription.is_some() {
                            Err(anyhow!("Found an inscription with two text tags."))
                        } else {
                            if let Some(text) = text {
                                *inscription = Some(text);
                                Ok(())
                            } else {
                                Err(anyhow!("Found a text tag without text."))
                            }
                        }
                    }

                    _ => Err(anyhow!("Unexpected text tag.")),
                }
            } else {
                Err(anyhow!("Expected text."))
            }
        }
    }
}

pub(crate) mod tag_transition {
    use crate::ebi_objects::petri_net_markup_language::{
        namespace::NameSpace,
        parser_state::ParserState,
        parser_traits::{Closeable, Openable, Recognisable},
        tags::{OpenedTag, Tag},
    };
    use anyhow::{Result, anyhow};
    use quick_xml::events::{BytesEnd, BytesStart};

    pub struct TagTransition {}

    impl Recognisable for TagTransition {
        fn recognise_tag(e: &BytesStart, state: &ParserState, n: NameSpace) -> Option<Tag>
        where
            Self: Sized,
        {
            if n.is_pnml() {
                match state.open_tags.iter().last() {
                    Some(OpenedTag::Page { .. }) => {
                        if e.local_name().as_ref() == "transition" {
                            return Some(Tag::Transition);
                        }
                    }
                    _ => (),
                }
                None
            } else {
                None
            }
        }
    }

    impl Openable for TagTransition {
        fn open_tag(_tag: Tag, e: &BytesStart, state: &mut ParserState) -> Result<OpenedTag>
        where
            Self: Sized,
        {
            let id = state.read_id(e)?;

            Ok(OpenedTag::Transition {
                id,
                invisible_tag: false,
                label: None,
            })
        }
    }

    impl Closeable for TagTransition {
        fn close_tag(opened_tag: OpenedTag, _e: &BytesEnd, state: &mut ParserState) -> Result<()> {
            if let Some(OpenedTag::Page {
                lpn,
                transition_id_2_rank,
                ..
            }) = state.open_tags.iter_mut().last()
            {
                if let OpenedTag::Transition {
                    id,
                    invisible_tag,
                    label,
                    ..
                } = opened_tag
                {
                    let transition_rank = if let Some(label) = label
                        && !invisible_tag
                    {
                        let activity = lpn.activity_key.process_activity(&label);
                        lpn.add_transition(Some(activity))
                    } else {
                        //silent
                        lpn.add_transition(None)
                    };

                    transition_id_2_rank.insert(id, transition_rank);
                }
                Ok(())
            } else {
                return Err(anyhow!("Expected a page."));
            }
        }
    }
}

pub(crate) mod tag_name {

    use crate::ebi_objects::petri_net_markup_language::{
        namespace::NameSpace,
        parser_state::ParserState,
        parser_traits::{Closeable, Openable, Recognisable},
        tags::{OpenedTag, Tag},
    };
    use anyhow::{Result, anyhow};
    use quick_xml::events::{BytesEnd, BytesStart};

    pub struct TagName {}

    impl Recognisable for TagName {
        fn recognise_tag(e: &BytesStart, state: &ParserState, n: NameSpace) -> Option<Tag>
        where
            Self: Sized,
        {
            if n.is_pnml() {
                match state.open_tags.iter().last() {
                    Some(OpenedTag::Transition { .. }) => {
                        if e.local_name().as_ref() == "name" {
                            return Some(Tag::Name);
                        }
                    }
                    _ => (),
                }
                None
            } else {
                None
            }
        }
    }

    impl Openable for TagName {
        fn open_tag(_tag: Tag, _e: &BytesStart, _state: &mut ParserState) -> Result<OpenedTag>
        where
            Self: Sized,
        {
            Ok(OpenedTag::Name { name: None })
        }
    }

    impl Closeable for TagName {
        fn close_tag(opened_tag: OpenedTag, _e: &BytesEnd, state: &mut ParserState) -> Result<()> {
            //find the page
            if let Some(OpenedTag::Transition { label, .. }) = state.open_tags.iter_mut().last() {
                if let OpenedTag::Name { name, .. } = opened_tag {
                    *label = name;
                }
                Ok(())
            } else {
                return Err(anyhow!("Expected a page."));
            }
        }
    }
}

pub(crate) mod tag_tool_specific {
    use crate::ebi_objects::petri_net_markup_language::{
        namespace::NameSpace,
        parser_state::{ParserState, parse_attribute},
        parser_traits::{Closeable, Openable, Recognisable},
        tags::{OpenedTag, Tag},
    };
    use anyhow::{Result, anyhow};
    use quick_xml::events::{BytesEnd, BytesStart};

    pub struct TagToolSpecific {}

    impl Recognisable for TagToolSpecific {
        fn recognise_tag(e: &BytesStart, state: &ParserState, n: NameSpace) -> Option<Tag>
        where
            Self: Sized,
        {
            if n.is_pnml() {
                match state.open_tags.iter().last() {
                    Some(OpenedTag::Transition { .. }) => {
                        if e.local_name().as_ref() == "toolspecific" {
                            //tool must be ProM
                            if parse_attribute(e, "tool") != Some("ProM".to_string()) {
                                return None;
                            }

                            //activity must be invisible
                            if parse_attribute(e, "activity") != Some("$invisible$".to_string()) {
                                return None;
                            }

                            return Some(Tag::ToolSpecific);
                        }
                    }
                    _ => (),
                }
                None
            } else {
                None
            }
        }
    }

    impl Openable for TagToolSpecific {
        fn open_tag(_tag: Tag, _e: &BytesStart, _state: &mut ParserState) -> Result<OpenedTag>
        where
            Self: Sized,
        {
            Ok(OpenedTag::ToolSpecific {})
        }
    }

    impl Closeable for TagToolSpecific {
        fn close_tag(opened_tag: OpenedTag, _e: &BytesEnd, state: &mut ParserState) -> Result<()> {
            //find the page
            if let Some(OpenedTag::Transition { invisible_tag, .. }) =
                state.open_tags.iter_mut().last()
            {
                if let OpenedTag::ToolSpecific { .. } = opened_tag {
                    *invisible_tag = true;
                }
                Ok(())
            } else {
                return Err(anyhow!("Expected a transition."));
            }
        }
    }
}

pub(crate) mod tag_arc {
    use crate::ebi_objects::petri_net_markup_language::{
        namespace::NameSpace,
        parser_state::{ParserState, parse_attribute},
        parser_traits::{Closeable, Openable, Recognisable},
        tags::{OpenedTag, Tag},
    };
    use anyhow::{Context, Result, anyhow};
    use quick_xml::events::{BytesEnd, BytesStart};

    #[derive(Debug)]
    pub struct DraftArc {
        pub(crate) id: String,
        pub(crate) source_id: String,
        pub(crate) target_id: String,
        pub(crate) cardinality: u64,
    }

    pub struct TagArc {}

    impl Recognisable for TagArc {
        fn recognise_tag(e: &BytesStart, state: &ParserState, n: NameSpace) -> Option<Tag>
        where
            Self: Sized,
        {
            if n.is_pnml() {
                match state.open_tags.iter().last() {
                    Some(OpenedTag::Page { .. }) => {
                        if e.local_name().as_ref() == "arc" {
                            return Some(Tag::Arc);
                        }
                    }
                    _ => (),
                }
                None
            } else {
                None
            }
        }
    }

    impl Openable for TagArc {
        fn open_tag(_tag: Tag, e: &BytesStart, state: &mut ParserState) -> Result<OpenedTag>
        where
            Self: Sized,
        {
            let id = state
                .read_id(e)
                .with_context(|| anyhow!("Arc does not have an id."))?;

            let source_id = parse_attribute(e, "source")
                .ok_or_else(|| anyhow!("The arc `{}` has no source.", id))?;
            let target_id = parse_attribute(e, "target")
                .ok_or_else(|| anyhow!("The arc `{}` has no target.", id))?;

            Ok(OpenedTag::Arc {
                id,
                source_id,
                target_id,
                cardinality: None,
            })
        }
    }

    impl Closeable for TagArc {
        fn close_tag(opened_tag: OpenedTag, _e: &BytesEnd, state: &mut ParserState) -> Result<()> {
            //find the page
            if let Some(OpenedTag::Page { draft_arcs, .. }) = state.open_tags.iter_mut().last() {
                if let OpenedTag::Arc {
                    id,
                    source_id,
                    target_id,
                    cardinality,
                } = opened_tag
                {
                    draft_arcs.push(DraftArc {
                        id,
                        source_id,
                        target_id,
                        cardinality: if let Some(c) = cardinality { c } else { 1 },
                    });
                }
                Ok(())
            } else {
                return Err(anyhow!("Expected a transition."));
            }
        }
    }
}

pub(crate) mod tag_inscription {
    use crate::ebi_objects::petri_net_markup_language::{
        namespace::NameSpace,
        parser_state::ParserState,
        parser_traits::{Closeable, Openable, Recognisable},
        tags::{OpenedTag, Tag},
    };
    use anyhow::{Context, Result, anyhow};
    use quick_xml::events::{BytesEnd, BytesStart};

    pub struct TagInscription {}

    impl Recognisable for TagInscription {
        fn recognise_tag(e: &BytesStart, state: &ParserState, n: NameSpace) -> Option<Tag>
        where
            Self: Sized,
        {
            if n.is_pnml() {
                match state.open_tags.iter().last() {
                    Some(OpenedTag::Arc { .. }) => {
                        if e.local_name().as_ref() == "inscription" {
                            return Some(Tag::Inscription);
                        }
                    }
                    _ => (),
                }
                None
            } else {
                None
            }
        }
    }

    impl Openable for TagInscription {
        fn open_tag(_tag: Tag, _e: &BytesStart, _state: &mut ParserState) -> Result<OpenedTag>
        where
            Self: Sized,
        {
            Ok(OpenedTag::Inscription { inscription: None })
        }
    }

    impl Closeable for TagInscription {
        fn close_tag(opened_tag: OpenedTag, _e: &BytesEnd, state: &mut ParserState) -> Result<()> {
            //find the page
            if let Some(OpenedTag::Arc {
                id, cardinality, ..
            }) = state.open_tags.iter_mut().last()
            {
                if let OpenedTag::Inscription { inscription } = opened_tag {
                    if let Some(inscription) = inscription {
                        if cardinality.is_some() {
                            return Err(anyhow!(
                                "Arc `{}` has two cardinalities, which may be given as arc inscriptions",
                                id
                            ));
                        }

                        let c = inscription.parse::<u64>().with_context(|| anyhow!("Arc `{}` has an inscription that cannot be parsed as a number. To preven misinterpretation, Ebi will not import this PNML file.", id))?;

                        *cardinality = Some(c);
                    } else {
                        return Err(anyhow!(
                            "Empty inscription found on arc `{}`. An arc inscription should have a text tag with the cardinality of the arc.",
                            id
                        ));
                    }
                }
                Ok(())
            } else {
                return Err(anyhow!("Expected an arc."));
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::PetriNetMarkupLanguage;
    use std::fs;

    #[test]
    fn parse_a() {
        let fin = fs::read_to_string("testfiles/a.pnml").unwrap();
        let pnml = fin.parse::<PetriNetMarkupLanguage>().unwrap();

        dbg!(pnml);
    }

    #[test]
    fn parse_xor_tau_a() {
        let fin = fs::read_to_string("testfiles/xor(tau,a).pnml").unwrap();
        let pnml = fin.parse::<PetriNetMarkupLanguage>().unwrap();

        dbg!(pnml);
    }

    #[test]
    fn parse_bpic12() {
        let fin = fs::read_to_string("testfiles/bpic12-a.apnml").unwrap();
        let pnml = fin.parse::<PetriNetMarkupLanguage>().unwrap();

        dbg!(pnml);
    }
}
