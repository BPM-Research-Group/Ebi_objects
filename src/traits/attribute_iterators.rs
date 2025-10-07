use crate::{
    Attribute,
    iterators::{
        attribute_iterator::{
            CategoricalAttributeIterator, NumericAttributeIterator, TimeAttributeIterator,
        },
        trace_iterator::TraceIterator,
    },
};

pub trait IntoAttributeIterator {
    /// Iterator over the categorical attribute values of traces,
    /// i.e. for traces that do not have the attribute set, a None is included.
    /// A call to .flatten() transforms this to remove those values.
    fn iter_categorical(&self, attribute: Attribute) -> CategoricalAttributeIterator<'_>;

    /// Iterator over tbe numeric attribute values of traces,
    /// i.e. for traces that do not have the attribute set, a None is included.
    /// A call to .flatten() transforms this to remove those values.
    fn iter_numeric(&self, attribute: Attribute) -> NumericAttributeIterator<'_>;

    /// Iterator over the time attribute values of traces,
    /// i.e. for traces that do not have the attribute set, a None is included.
    /// A call to .flatten() transforms this to remove those values.
    fn iter_time(&self, attribute: Attribute) -> TimeAttributeIterator<'_>;
}

pub trait IntoAttributeTraceIterator {
    /// Iterator over traces and their time attribute values,
    /// i.e. for traces that do not have the attribute set, a None is included.
    /// A call to .flatten() transforms this to remove those values.
    fn iter_categorical_and_traces(
        &self,
        attribute: Attribute,
    ) -> std::iter::Zip<TraceIterator<'_>, CategoricalAttributeIterator<'_>>;

    /// Iterator over traces and their numeric attribute values,
    /// i.e. for traces that do not have the attribute set, a None is included.
    /// A call to .flatten() transforms this to remove those values.
    fn iter_numeric_and_traces(
        &self,
        attribute: Attribute,
    ) -> std::iter::Zip<TraceIterator<'_>, NumericAttributeIterator<'_>>;

    /// Iterator over traces and their time attribute values,
    /// i.e. for traces that do not have the attribute set, a None is included.
    /// A call to .flatten() transforms this to remove those values.
    fn iter_time_and_traces(
        &self,
        attribute: Attribute,
    ) -> std::iter::Zip<TraceIterator<'_>, TimeAttributeIterator<'_>>;
}
