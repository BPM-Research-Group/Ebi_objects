use crate::{Attribute, DataType, Infoable};
use process_mining::event_log::AttributeValue;
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

    pub fn process_attribute(
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
