use crate::activity_key::{
    activity_key::ActivityKey, translate_activity_key::TranslateActivityKey,
};

#[cfg(test)]
pub trait HasActivityKey: TranslateActivityKey + TestActivityKey {
    fn activity_key(&self) -> &ActivityKey;

    fn activity_key_mut(&mut self) -> &mut ActivityKey;
}

#[cfg(not(test))]
pub trait HasActivityKey: TranslateActivityKey {
    fn activity_key(&self) -> &ActivityKey;

    fn activity_key_mut(&mut self) -> &mut ActivityKey;
}

#[cfg(test)]
pub trait TestActivityKey {
    /// The implementation should call self.activity_key().assert_activity_is_of_key() on each activity contained in the object.
    fn test_activity_key(&self);
}
