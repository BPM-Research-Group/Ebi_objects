use crate::activity_key::{activity_key::ActivityKey, translate_activity_key::TranslateActivityKey};

pub trait HasActivityKey: TranslateActivityKey {
    fn activity_key(&self) -> &ActivityKey;

    fn activity_key_mut(&mut self) -> &mut ActivityKey;
}