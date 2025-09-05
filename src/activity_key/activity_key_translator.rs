use crate::{Activity, ActivityKey};

pub struct ActivityKeyTranslator {
    from2to: Vec<Activity>,
}

impl ActivityKeyTranslator {
    pub fn new(from: &ActivityKey, to: &mut ActivityKey) -> Self {
        let mut from2to = vec![];

        for label_from in &from.activity2name {
            let index_to = to.process_activity(&label_from);
            from2to.push(index_to);
        }

        Self { from2to: from2to }
    }

    pub fn translate_activity(&self, activity: &Activity) -> Activity {
        self.from2to[activity.id]
    }

    pub fn translate_trace(&self, trace: &Vec<Activity>) -> Vec<Activity> {
        let mut result = Vec::with_capacity(trace.len());
        for from in trace {
            result.push(self.from2to[from.id]);
        }
        result
    }

    pub fn translate_trace_mut(&self, trace: &mut Vec<Activity>) {
        trace
            .iter_mut()
            .for_each(|event| *event = self.translate_activity(event));
    }
}
