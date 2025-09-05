#[cfg(test)]
mod tests {
    use std::fs;

    use crate::{ebi_objects::directly_follows_model::DirectlyFollowsModel, ActivityKey, TranslateActivityKey};

    #[test]
    fn activity_key_translating() {
        let fin = fs::read_to_string("testfiles/a-b_star.dfm").unwrap();
        let mut dfm = fin.parse::<DirectlyFollowsModel>().unwrap();

        let mut activity_key = ActivityKey::new();
        let x = activity_key.process_activity("xyz");

        dfm.translate_using_activity_key(&mut activity_key);

        assert_eq!(
            dfm.get_activity_key().get_activity_label(&x),
            activity_key.get_activity_label(&x)
        );
    }
}
