use crate::activity_key::activity_key::ActivityKey;

pub trait TranslateActivityKey {
    /**
     * Change the activity key of this object, by translating all mentions of activities to the new activity key (which will be updated with activity labels it did not have yet.).
     * This is a potentially expensive operation. If only a part of the activities will be used, then consider using an ActivityKeyTranslator directly.
     * The activity key of this object will be updated too, so the activity keys will be equivalent afterwards.
     */
    fn translate_using_activity_key(&mut self, to_activity_key: &mut ActivityKey);
}