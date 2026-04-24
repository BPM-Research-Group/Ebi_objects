use anyhow::Context;
use chrono::{DateTime, FixedOffset};
use ebi_arithmetic::Fraction;
use ebi_arithmetic::anyhow::{Result, anyhow};
use serde_json::{Map, Value};

pub fn read_field_bool(json: &Value, field: &str) -> Result<bool> {
    match &json[field] {
        Value::Null => Err(anyhow!("field not found")),
        Value::Bool(b) => Ok(*b),
        Value::Number(_) => Err(anyhow!("field is a number, where boolean expected")),
        Value::String(_) => Err(anyhow!("field is a literal, where number expected")),
        Value::Array(_) => Err(anyhow!("field is a list, where number expected")),
        Value::Object(_) => Err(anyhow!("field is an object, where number expected")),
    }
}

pub fn read_field_index(json: &Value, field: &str) -> Result<usize> {
    match &json[field] {
        Value::Null => return Err(anyhow!("field not found")),
        Value::Bool(_) => return Err(anyhow!("field is a boolean, where number expected")),
        Value::Number(n) => {
            if !n.is_u64() {
                return Err(anyhow!("number is not an integer"));
            }
            return Ok(usize::try_from(n.as_u64().unwrap())?);
        }
        Value::String(s) => return Ok(s.parse::<usize>()?),
        Value::Array(_) => return Err(anyhow!("field is a list, where number expected")),
        Value::Object(_) => return Err(anyhow!("field is an object, where number expected")),
    }
}

pub fn read_field_fraction(json: &Value, field: &str) -> Result<Fraction> {
    match &json[field] {
        Value::Null => return Err(anyhow!("field not found")),
        Value::Bool(_) => return Err(anyhow!("field is a boolean, where fraction expected")),
        Value::Number(n) => return Ok(n.to_string().parse::<Fraction>()?),
        Value::String(s) => return Ok(s.parse::<Fraction>()?),
        Value::Array(_) => return Err(anyhow!("field is a list, where fraction expected")),
        Value::Object(_) => return Err(anyhow!("field is an object, where fraction expected")),
    }
}

pub fn read_field_list<'a>(json: &'a Value, field: &str) -> Result<&'a Vec<Value>> {
    match &json[field] {
        Value::Null => return Err(anyhow!("field not found")),
        Value::Bool(_) => return Err(anyhow!("field is a boolean, where list expected")),
        Value::Number(_) => return Err(anyhow!("field is a number, where list expected")),
        Value::String(_) => return Err(anyhow!("field is a literal, where list expected")),
        Value::Array(arr) => return Ok(&arr),
        Value::Object(_) => return Err(anyhow!("field is an object, where list expected")),
    }
}

pub fn read_field_object<'a>(json: &'a Value, field: &str) -> Result<&'a Map<String, Value>> {
    match &json[field] {
        Value::Null => return Err(anyhow!("field not found")),
        Value::Bool(_) => return Err(anyhow!("field is a boolean, where object expected")),
        Value::Number(_) => return Err(anyhow!("field is a number, where object expected")),
        Value::String(_) => return Err(anyhow!("field is a literal, where object expected")),
        Value::Array(_) => return Err(anyhow!("field is an array, where object expected")),
        Value::Object(obj) => Ok(&obj),
    }
}

pub fn read_field_string<'a>(json: &'a Value, field: &str) -> Result<String> {
    match &json[field] {
        Value::Null => return Err(anyhow!("field not found")),
        Value::Bool(_) => return Err(anyhow!("field is a boolean, where literal expected")),
        Value::Number(n) => return Ok(n.to_string()),
        Value::String(s) => return Ok(s.to_string()),
        Value::Array(_) => return Err(anyhow!("field is a list, where literal expected")),
        Value::Object(_) => return Err(anyhow!("field is an object, where literal expected")),
    }
}

pub fn read_field_index_or_null(json: &Value, field: &str) -> Result<Option<usize>> {
    match &json[field] {
        Value::Null => return Ok(None),
        Value::Bool(_) => return Err(anyhow!("field is a boolean, where number expected")),
        Value::Number(n) => {
            if !n.is_u64() {
                return Err(anyhow!("number is not an integer"));
            }
            return Ok(Some(usize::try_from(n.as_u64().unwrap())?));
        }
        Value::String(s) => return Ok(Some(s.parse::<usize>()?)),
        Value::Array(_) => return Err(anyhow!("field is a list, where number expected")),
        Value::Object(_) => return Err(anyhow!("field is an object, where number expected")),
    }
}

pub fn read_field_fraction_or_null(json: &Value, field: &str) -> Result<Option<Fraction>> {
    match &json[field] {
        Value::Null => return Ok(None),
        Value::Bool(_) => return Err(anyhow!("field is a boolean, where fraction expected")),
        Value::Number(n) => return Ok(Some(n.to_string().parse::<Fraction>()?)),
        Value::String(s) => return Ok(Some(s.parse::<Fraction>()?)),
        Value::Array(_) => return Err(anyhow!("field is a list, where fraction expected")),
        Value::Object(_) => return Err(anyhow!("field is an object, where fraction expected")),
    }
}

pub fn read_field_string_or_null<'a>(json: &'a Value, field: &str) -> Result<Option<String>> {
    match &json[field] {
        Value::Null => return Ok(None),
        Value::Bool(_) => return Err(anyhow!("field is a boolean, where literal expected")),
        Value::Number(n) => return Ok(Some(n.to_string())),
        Value::String(s) => return Ok(Some(s.to_string())),
        Value::Array(_) => return Err(anyhow!("field is a list, where literal expected")),
        Value::Object(_) => return Err(anyhow!("field is an object, where literal expected")),
    }
}

pub fn read_field_time_or_null<'a>(
    json: &'a Value,
    field: &str,
) -> Result<Option<DateTime<FixedOffset>>> {
    match &json[field] {
        Value::Null => return Ok(None),
        Value::Bool(_) => return Err(anyhow!("Field is a boolean, where a time was expected.")),
        Value::Number(_) => return Err(anyhow!("Field is a number, where a time was expected.")),
        Value::String(s) => {
            return Ok(Some(
                s.parse()
                    .with_context(|| anyhow!("field cannot be parsed as a time."))?,
            ));
        }
        Value::Array(_) => return Err(anyhow!("Field is a list, where a time was expected.")),
        Value::Object(_) => return Err(anyhow!("Field is an object, where a time was expected.")),
    }
}

pub fn read_index(json: &Value) -> Result<usize> {
    match &json {
        Value::Null => return Err(anyhow!("field not found")),
        Value::Bool(_) => return Err(anyhow!("field is a boolean, where number expected")),
        Value::Number(n) => {
            if !n.is_u64() {
                return Err(anyhow!("number is not an integer"));
            }
            return Ok(usize::try_from(n.as_u64().unwrap())?);
        }
        Value::String(_) => return Err(anyhow!("field is a literal, where number expected")),
        Value::Array(_) => return Err(anyhow!("field is a list, where number expected")),
        Value::Object(_) => return Err(anyhow!("field is an object, where number expected")),
    }
}

pub fn read_string(json: &Value) -> Result<&String> {
    match &json {
        Value::Null => return Err(anyhow!("field not found")),
        Value::Bool(_) => return Err(anyhow!("field is a boolean, where literal expected")),
        Value::Number(_) => return Err(anyhow!("field is a number, where literal expected")),
        Value::String(s) => return Ok(s),
        Value::Array(_) => return Err(anyhow!("field is a list, where literal expected")),
        Value::Object(_) => return Err(anyhow!("field is an object, where literal expected")),
    }
}

pub fn read_list(json: &Value) -> Result<&Vec<Value>> {
    match &json {
        Value::Null => return Err(anyhow!("field not found")),
        Value::Bool(_) => return Err(anyhow!("field is a boolean, where list expected")),
        Value::Number(_) => return Err(anyhow!("field is a number, where list expected")),
        Value::String(_) => return Err(anyhow!("field is a literal, where list expected")),
        Value::Array(s) => return Ok(s),
        Value::Object(_) => return Err(anyhow!("field is an object, where literal expected")),
    }
}

pub fn read_fraction(json: &Value) -> Result<Fraction> {
    match &json {
        Value::Null => return Err(anyhow!("field not found")),
        Value::Bool(_) => return Err(anyhow!("field is a boolean, where fraction expected")),
        Value::Number(n) => return Ok(n.to_string().parse::<Fraction>()?),
        Value::String(s) => return Ok(s.parse::<Fraction>()?),
        Value::Array(_) => return Err(anyhow!("field is a list, where fraction expected")),
        Value::Object(_) => return Err(anyhow!("field is an object, where fraction expected")),
    }
}
