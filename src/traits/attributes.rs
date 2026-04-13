use chrono::{DateTime, FixedOffset};
use ebi_arithmetic::Fraction;
use intmap::IntMap;
use process_mining::core::event_data::case_centric::AttributeValue;

use crate::Attribute;

pub trait Attributes {
    fn get_attribute_categorical(&self, attribute: Attribute) -> Option<String>;

    fn get_attribute_time(&self, attribute: Attribute) -> Option<DateTime<FixedOffset>>;

    fn get_attribute_numeric(&self, attribute: Attribute) -> Option<Fraction>;
}

impl Attributes for IntMap<Attribute, AttributeValue> {
    fn get_attribute_categorical(&self, attribute: Attribute) -> Option<String> {
        match self.get(attribute)? {
            AttributeValue::String(x) => Some(x.to_owned()),
            AttributeValue::Date(_) => None,
            AttributeValue::Int(x) => Some(x.to_string()),
            AttributeValue::Float(x) => Some(x.to_string()),
            AttributeValue::Boolean(x) => Some(x.to_string()),
            AttributeValue::ID(_) => None,
            AttributeValue::List(_) => None,
            AttributeValue::Container(_) => None,
            AttributeValue::None() => None,
        }
    }

    fn get_attribute_time(&self, attribute: Attribute) -> Option<DateTime<FixedOffset>> {
        match self.get(attribute)? {
            AttributeValue::String(x) => Some(x.parse::<DateTime<FixedOffset>>().ok()?),
            AttributeValue::Date(x) => Some(*x),
            AttributeValue::Int(_) => None,
            AttributeValue::Float(_) => None,
            AttributeValue::Boolean(_) => None,
            AttributeValue::ID(_) => None,
            AttributeValue::List(_) => None,
            AttributeValue::Container(_) => None,
            AttributeValue::None() => None,
        }
    }

    fn get_attribute_numeric(&self, attribute: Attribute) -> Option<Fraction> {
        match self.get(attribute)? {
            AttributeValue::String(x) => Some(x.parse::<Fraction>().ok()?),
            AttributeValue::Date(_) => None,
            AttributeValue::Int(x) => Some(Fraction::from(*x)),
            AttributeValue::Float(x) => Some(x.to_string().parse::<Fraction>().ok()?),
            AttributeValue::Boolean(_) => None,
            AttributeValue::ID(_) => None,
            AttributeValue::List(_) => None,
            AttributeValue::Container(_) => None,
            AttributeValue::None() => None,
        }
    }
}
