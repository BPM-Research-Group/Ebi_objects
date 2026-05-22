use crate::{
    Attribute,
    iterators::event_attribute_iterator::{
        CategoricalEventAttributeIterator, NumericEventAttributeIterator,
        TimeEventAttributeIterator,
    },
};

pub trait IntoEventAttributeIterator {
    /// Iterator over traces, with access to categorical event attribute values,
    /// i.e. for events that do not have the attribute set, a None is included.
    /// A call to .flatten() transforms this to remove those values.
    fn iter_categorical_attribute_and_events(
        &self,
        attribute: Attribute,
    ) -> CategoricalEventAttributeIterator<'_>;

    /// Iterator over traces, with access to numeric event attribute values,
    /// i.e. for events that do not have the attribute set, a None is included.
    /// A call to .flatten() transforms this to remove those values.
    fn iter_numeric_attribute_and_events(
        &self,
        attribute: Attribute,
    ) -> NumericEventAttributeIterator<'_>;

    /// Iterator over traces, with access to time event attribute values,
    /// i.e. for events that do not have the attribute set, a None is included.
    /// A call to .flatten() transforms this to remove those values.
    fn iter_time_attribute_and_events(
        &self,
        attribute: Attribute,
    ) -> TimeEventAttributeIterator<'_>;

    /// Iterator over traces, with access to timestamps,
    /// i.e. for events that do not have the time attribute set, a None is included.
    /// A call to .flatten() transforms this to remove those values.
    /// If no time attribute was set during import, returns None.
    fn iter_time_and_events(&self) -> Option<TimeEventAttributeIterator<'_>>;

    /// Iterator over traces, with access to resources,
    /// i.e. for events that do not have the attribute set, a None is included.
    /// A call to .flatten() transforms this to remove those values.
    /// If no resource attribute was set during import, returns None.
    fn iter_time_and_resources(&self) -> Option<CategoricalEventAttributeIterator<'_>>;
}
