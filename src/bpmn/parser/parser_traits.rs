use crate::bpmn::parser::{
    parser_state::ParserState,
    tags::{OpenedTag, Tag},
};
use anyhow::Result;
use quick_xml::events::{BytesEnd, BytesStart};

pub(crate) trait Recognisable {
    /// Given a start tag, determine whether it is recognisable
    fn recognise_tag(e: &BytesStart, state: &ParserState) -> Option<Tag>
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
