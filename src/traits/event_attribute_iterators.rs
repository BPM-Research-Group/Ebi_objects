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
    fn iter_categorical_and_events(
        &self,
        attribute: Attribute,
    ) -> CategoricalEventAttributeIterator<'_>;

    /// Iterator over traces, with access to numeric event attribute values,
    /// i.e. for events that do not have the attribute set, a None is included.
    /// A call to .flatten() transforms this to remove those values.
    fn iter_numeric_and_events(&self, attribute: Attribute) -> NumericEventAttributeIterator<'_>;

    /// Iterator over traces, with access to time event attribute values,
    /// i.e. for events that do not have the attribute set, a None is included.
    /// A call to .flatten() transforms this to remove those values.
    fn iter_time_and_events(&self, attribute: Attribute) -> TimeEventAttributeIterator<'_>;
}
