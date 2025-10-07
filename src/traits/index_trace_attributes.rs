use crate::{
    Attribute,
    iterators::trace_iterator::{
        EventLogTraceAttributeCategoricalIterator, EventLogTraceAttributeTraceCategoricalIterator,
    },
};

pub trait IndexTraceAttributes {
    /// Iterate over the traces that have this attribute.
    fn iter_traces_categorical(
        &self,
        attribute: Attribute,
    ) -> EventLogTraceAttributeTraceCategoricalIterator<'_>;

    fn iter_categorical(
        &self,
        attribute: Attribute,
    ) -> EventLogTraceAttributeCategoricalIterator<'_>;
}
