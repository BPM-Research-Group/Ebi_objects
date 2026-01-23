use crate::{Attribute, DataType, Infoable};
use process_mining::core::event_data::case_centric::AttributeValue;
use rayon::iter::{IndexedParallelIterator, IntoParallelIterator, IntoParallelRefIterator};
use std::{borrow::Borrow, collections::HashMap};

#[derive(Clone, Debug)]
pub struct AttributeKey {
    pub(crate) name2attribute: HashMap<String, Attribute>,
    pub(crate) attribute2name: Vec<String>,
    pub(crate) attribute2type: Vec<DataType>,
}

impl<'a> AttributeKey {
    pub fn new() -> Self {
        Self {
            name2attribute: HashMap::new(),
            attribute2name: vec![],
            attribute2type: vec![],
        }
    }

    pub fn attribute_to_label(&self, attribute: impl Borrow<Attribute>) -> Option<&String> {
        self.attribute2name.get(attribute.borrow().id)
    }

    pub fn label_to_attribute(&self, label: &str) -> Option<Attribute> {
        self.name2attribute.get(label).copied()
    }

    pub fn id_to_attribute(&self, attribute_id: usize) -> Attribute {
        Attribute { id: attribute_id }
    }

    pub fn attribute_to_id(&self, attribute: impl Borrow<Attribute>) -> usize {
        attribute.borrow().id
    }

    pub fn attribute_to_data_type(&self, attribute: impl Borrow<Attribute>) -> Option<&DataType> {
        self.attribute2type.get(attribute.borrow().id)
    }

    pub fn set_label(&mut self, attribute: impl Borrow<Attribute>, label: &str) {
        if let Some(old_label) = self.attribute2name.get(attribute.borrow().id) {
            let old_label = old_label.to_string();
            self.attribute2name[attribute.borrow().id] = label.to_string();
            self.name2attribute.remove(&old_label);
            self.name2attribute
                .insert(label.to_string(), attribute.borrow().clone());
        }
    }

    #[cfg(test)]
    pub fn add_categorical_attribute(&mut self, label: &str) -> Attribute {
        let next_index = self.name2attribute.len();
        match self.name2attribute.get(label) {
            Some(attribute) => *attribute,
            None => {
                let result = Attribute { id: next_index };
                self.attribute2name.push(label.to_string());
                self.name2attribute.insert(label.to_string(), result);
                self.attribute2type.push(DataType::Categorical);
                return result;
            }
        }
    }

    ///Must be called consecutively for the first row.
    pub fn process_attribute_column(
        &mut self,
        attribute_id: usize,
        attribute_value: &str,
    ) -> Attribute {
        if attribute_id == self.name2attribute.len() {
            let id = self.attribute2name.len();
            self.attribute2name.push(id.to_string());
            self.name2attribute.insert(id.to_string(), Attribute { id });
            self.attribute2type
                .push(DataType::init(&AttributeValue::String(
                    attribute_value.to_string(),
                )));
            self.id_to_attribute(id)
        } else {
            self.attribute2type[attribute_id]
                .update(&AttributeValue::String(attribute_value.to_string()));
            self.id_to_attribute(attribute_id)
        }
    }

    pub fn process_attribute_value(
        &mut self,
        attribute_name: &str,
        attribute_value: &AttributeValue,
    ) -> Attribute {
        let next_index = self.name2attribute.len();
        match self.name2attribute.get(attribute_name) {
            Some(index) => {
                self.attribute2type[index.id].update(attribute_value);
                return *index;
            }
            None => {
                let result = Attribute { id: next_index };
                self.attribute2name.push(attribute_name.to_string());
                self.name2attribute
                    .insert(attribute_name.to_string(), result);
                self.attribute2type.push(DataType::init(&attribute_value));
                return result;
            }
        }
    }

    pub fn len(&self) -> usize {
        self.attribute2name.len()
    }
}

impl Infoable for AttributeKey {
    fn info(&self, f: &mut impl std::io::Write) -> anyhow::Result<()> {
        //make tuples
        let mut tuples = self
            .attribute2name
            .iter()
            .zip(self.attribute2type.iter())
            .collect::<Vec<_>>();
        tuples.sort_by(|a, b| a.0.cmp(b.0));

        for (label, typee) in tuples {
            writeln!(f, "\t{}\t{}", label, typee)?;
        }

        Ok(write!(f, "")?)
    }
}

impl<'a> IntoParallelIterator for &'a AttributeKey {
    type Iter = rayon::iter::Zip<rayon::vec::IntoIter<Attribute>, rayon::slice::Iter<'a, DataType>>;
    type Item = (Attribute, &'a DataType);

    fn into_par_iter(self) -> Self::Iter {
        let x = (0..self.len())
            .map(|id| self.id_to_attribute(id))
            .collect::<Vec<_>>();

        x.into_par_iter().zip(self.attribute2type.par_iter())
    }
}
