use crate::AttributeKey;

pub trait HasAttributeKey {
    fn attribute_key(&self) -> &AttributeKey;

    fn attribute_key_mut(&mut self) -> &mut AttributeKey;
}
