use crate::bpmn::parser::{
    parser_state::ParserState,
    parser_traits::{Closeable, Openable, Recognisable},
    tags::{OpenedTag, Tag},
};
use anyhow::{Context, Result, anyhow};
use quick_xml::{
    events::{BytesEnd, BytesStart},
    name::{Namespace, ResolveResult},
};

pub(crate) fn empty_tag(state: &mut ParserState, e: &BytesStart) -> Result<()> {
    open_tag(state, e)?;
    close_tag(state, &e.to_end())
}

pub(crate) fn open_tag(state: &mut ParserState, e: &BytesStart) -> Result<()> {
    if let Some(tag) = Tag::recognise_tag(e, state) {
        let opened_tag =
            Tag::open_tag(tag, e, state).with_context(|| anyhow!("parsing tag `{}`", tag))?;
        state.open_tags.push(opened_tag);
    } else {
        state.open_tags.push(OpenedTag::Unknown);
    }

    state
        .open_tag_names
        .push(e.local_name().as_ref().to_owned());

    Ok(())
}

pub(crate) fn close_tag(state: &mut ParserState, e: &BytesEnd) -> Result<()> {
    if let (Some(most_recent_open_tag_name), Some(most_recent_open_tag)) =
        (state.open_tag_names.pop(), state.open_tags.pop())
    {
        if most_recent_open_tag_name == e.local_name().as_ref() {
            //closing tag matches last remaining opening tag

            OpenedTag::close_tag(most_recent_open_tag, e, state).with_context(|| {
                anyhow!(
                    "close tag `{}`",
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

pub(crate) fn is_in_namespace(result: ResolveResult) -> bool {
    match result {
        ResolveResult::Unbound => true,
        ResolveResult::Bound(Namespace(b"http://www.omg.org/spec/BPMN/20100524/MODEL")) => true,
        _ => false,
    }
}
