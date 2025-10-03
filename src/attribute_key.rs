use std::{borrow::Borrow, collections::HashMap};

use crate::Attribute;

#[derive(Clone, Debug)]
pub struct AttributeKey {
    pub name2attribute: HashMap<String, Attribute>,
    pub attribute2name: Vec<String>,
    pub next_index: usize,
}

impl<'a> AttributeKey {
    pub fn new() -> Self {
        Self {
            name2attribute: HashMap::new(),
            attribute2name: vec![],
            next_index: 0,
        }
    }

    pub fn get_attribute_by_id(&self, attribute_id: usize) -> Attribute {
        Attribute { id: attribute_id }
    }

    pub fn get_id_from_attribute(&self, attribute: impl Borrow<Attribute>) -> usize {
        attribute.borrow().id
    }

    pub fn process_attribute(&mut self, attribute: &str) -> Attribute {
        match self.name2attribute.get(attribute) {
            Some(index) => return *index,
            None => {
                let result = Attribute {
                    id: self.next_index,
                };
                self.attribute2name.push(attribute.to_string());
                self.name2attribute.insert(attribute.to_string(), result);
                self.next_index += 1;
                return result;
            }
        }
    }

    pub fn attribute_to_label(&self, attribute: Attribute) -> Option<&String> {
        self.attribute2name.get(attribute.id)
    }
}
