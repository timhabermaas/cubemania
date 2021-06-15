/// This module takes care of unmarshalling Ruby object decoded using `Marshal.dump` such as the
/// ActiveRecord storage.
use std::{collections::HashMap, error::Error};

#[derive(Debug)]
pub enum RubyObject {
    Hash(HashMap<RubyObject, RubyObject>),
    String(String),
    InstanceVariables(Box<RubyObject>, HashMap<String, RubyObject>),
    Symbol(String),
    Boolean(bool),
    IndexedSymbol(usize),
    Fixnum(i64),
}

impl std::hash::Hash for RubyObject {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            RubyObject::Hash(_) => {
                "hash".hash(state);
            }
            RubyObject::String(_) => {
                "string".hash(state);
            }
            RubyObject::InstanceVariables(_, _) => {
                "instance_variables".hash(state);
            }
            RubyObject::Symbol(_) => {
                "symbol".hash(state);
            }
            RubyObject::Boolean(_) => {
                "boolean".hash(state);
            }
            RubyObject::IndexedSymbol(_) => {
                "indexed_symbol".hash(state);
            }
            RubyObject::Fixnum(_) => {
                "fixnum".hash(state);
            }
        }
    }
}

impl PartialEq for RubyObject {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (RubyObject::Hash(a), RubyObject::Hash(b)) => a == b,
            (RubyObject::String(a), RubyObject::String(b)) => a == b,
            (RubyObject::InstanceVariables(a, c), RubyObject::InstanceVariables(b, d)) => {
                a == b && c == d
            }
            (RubyObject::Symbol(a), RubyObject::Symbol(b)) => a == b,
            (RubyObject::Boolean(a), RubyObject::Boolean(b)) => a == b,
            (RubyObject::IndexedSymbol(a), RubyObject::IndexedSymbol(b)) => a == b,
            (RubyObject::Fixnum(a), RubyObject::Fixnum(b)) => a == b,
            _ => false,
        }
    }
}

impl Eq for RubyObject {}

#[derive(Debug)]
struct DecodeError(String);

impl std::fmt::Display for DecodeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Something went wrong with decoding, {}", self.0)
    }
}

impl Error for DecodeError {}

pub fn decode(data: &[u8]) -> Result<RubyObject, Box<dyn Error>> {
    // Checking for version 4.8 of Ruby marshal format, see
    // https://docs.ruby-lang.org/en/2.1.0/marshal_rdoc.html#:~:text=refinements-,Marshal%20Format,used%20to%20serialize%20ruby%20objects.&text=This%20document%20calls%20a%20serialized,that%20implements%20a%20getc%20method.
    if data[0..=1] != [4, 8] {
        return Err(Box::new(DecodeError(
            "not matching version 4.8".to_string(),
        )));
    }

    let (_rest, result) = decode_object(&data[2..])?;
    Ok(result)
}

fn decode_object(data: &[u8]) -> Result<(&[u8], RubyObject), DecodeError> {
    match data[0] {
        b'{' => {
            let (rest, map) = decode_hash(&data[1..])?;
            return Ok((rest, RubyObject::Hash(map)));
        }
        b'"' => {
            let (rest, string) = decode_string(&data[1..])?;
            return Ok((rest, RubyObject::String(string)));
        }
        b'I' => {
            let (rest, object, variables) = decode_instance_variable(&data[1..])?;
            return Ok((
                rest,
                RubyObject::InstanceVariables(Box::new(object), variables),
            ));
        }
        b':' => {
            let (rest, symbol) = decode_symbol(&data[1..])?;
            return Ok((rest, RubyObject::Symbol(symbol)));
        }
        b'T' => {
            return Ok((&data[1..], RubyObject::Boolean(true)));
        }
        b'F' => {
            return Ok((&data[1..], RubyObject::Boolean(false)));
        }
        b';' => {
            let (rest, index) = decode_indexed_symbol(&data[1..])?;
            return Ok((rest, RubyObject::IndexedSymbol(index)));
        }
        b'i' => {
            let (rest, number) = decode_fixnum(&data[1..])?;
            return Ok((rest, RubyObject::Fixnum(number)));
        }
        _ => panic!("don't know how to decode {:?}", data[0]),
    }
}

fn decode_indexed_symbol(data: &[u8]) -> Result<(&[u8], usize), DecodeError> {
    decode_fixnum(data).map(|(r, x)| (r, x as usize))
}

fn decode_symbol(data: &[u8]) -> Result<(&[u8], String), DecodeError> {
    let (data, length) = decode_fixnum(data)?;
    let text = String::from_utf8_lossy(&data[0..length as usize]).into_owned();
    Ok((&data[length as usize..], text))
}

fn decode_instance_variable(
    data: &[u8],
) -> Result<(&[u8], RubyObject, HashMap<String, RubyObject>), DecodeError> {
    let (data, object) = decode_object(data)?;

    let (mut data, length) = decode_fixnum(data)?;
    println!("lenght: {}", length);
    for _ in 0..length {
        let (rest, variable_name) = decode_object(data)?;
        // TODO: Assert it's actually a symbol.

        println!("variable name: {:?}", variable_name);

        let (rest, value) = decode_object(rest)?;
        println!("variable value: {:?}", value);
        data = rest;
    }

    Ok((data, object, HashMap::new()))
}

fn decode_fixnum(data: &[u8]) -> Result<(&[u8], i64), DecodeError> {
    let first = data[0];
    if first == 0 {
        return Ok((&data[1..], 0));
    }
    if first > 4 {
        if first < 128 {
            return Ok((&data[1..], first as i64 - 5));
        } else if first < 252 {
            return Ok((&data[1..], first as i64 - 251));
        }
    }
    if first == 1 {
        return Ok((&data[2..], data[1] as i64));
    }
    if first == 255 {
        return Ok((&data[2..], 256 - data[1] as i64));
    }

    Err(DecodeError(format!(
        "no condition matched when parsing number, first byte: {:?}",
        first
    )))
}

fn decode_string(data: &[u8]) -> Result<(&[u8], String), DecodeError> {
    let (rest, length) = decode_fixnum(data)?;

    Ok((
        &rest[length as usize..],
        String::from_utf8_lossy(&rest[0..length as usize]).into_owned(),
    ))

    /*
    let stop_byte_index = data
        .iter()
        .position(|b| *b == 6)
        .ok_or(DecodeError("can't find stop byte for string".to_string()))?;
    let bytes = &data[0..stop_byte_index];
    Ok((
        &data[stop_byte_index..],
        String::from_utf8_lossy(bytes).into_owned(),
    ))*/
}

fn decode_hash(data: &[u8]) -> Result<(&[u8], HashMap<RubyObject, RubyObject>), DecodeError> {
    // TODO: Properly parse size of hash
    let (mut data, size) = dbg!(decode_fixnum(data)?);
    let mut map = HashMap::with_capacity(size as usize);
    for _ in 0..size {
        // key
        let (rest, key) = decode_object(data)?;
        let (rest, value) = decode_object(rest)?;
        map.insert(key, value);

        //return Err(DecodeError(format!("{:?} {:?}", key, value)));
        data = rest;
    }
    Ok((data, map))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple() {
        // "\x04\b{\aI\"\x10_csrf_token\x06:\x06EFI\"1ZtuLt1DYMgRJ9eNCv/Bjkk6dJU6MEywCXJaWUaXwyTs=\x06;\x00FI\"\fuser_id\x06;\x00Fi\b"
        /*let input: Vec<u8> = vec![
            4, 8, 123, 7, 73, 34, 16, 95, 99, 115, 114, 102, 95, 116, 111, 107, 101, 110, 6, 58, 6,
            69, 70, 73, 34, 49, 90, 116, 117, 76, 116, 49, 68, 89, 77, 103, 82, 74, 57, 101, 78,
            67, 118, 47, 66, 106, 107, 107, 54, 100, 74, 85, 54, 77, 69, 121, 119, 67, 88, 74, 97,
            87, 85, 97, 88, 119, 121, 84, 115, 61, 6, 59, 0, 70, 73, 34, 12, 117, 115, 101, 114,
            95, 105, 100, 6, 59, 0, 70, 105, 8,
        ];*/
        // \x04\b{\aI\"\x06a\x06:\x06ETI\"\x06c\x06;\x00TI\"\x06x\x06;\x00TI\"\x06y\x06;\x00T
        #[rustfmt::skip]
        let input: Vec<u8> = vec![
            // 04 08 7b 07 49 22 06 61 06 3a 06 45 54 49 22 06 63 06 3b 00 54 49 22 06 78 06 3b 00
            // 54 49 22 06 79 06 3b 00 54
            // version
            4, 8,
            123, // hash marker {
            7, // size: 2
            73, 34, 6, 97, 6, 58, 6, 69, 84, 73, 34, 6, 99, 6, 59, 0, 84, 73, 34, 6, 120, 6, 59,
            0, 84, 73, 34, 6, 121, 6, 59, 0, 84,
        ];
        let result = decode(&input);
        // expected: {"a" => "c", "x" => "y"}
        println!("{:?}", result);
        panic!("foo");
    }

    #[test]
    fn test_complex() {
        // "\x04\b{\aI\"\x10_csrf_token\x06:\x06EFI\"1ZtuLt1DYMgRJ9eNCv/Bjkk6dJU6MEywCXJaWUaXwyTs=\x06;\x00FI\"\fuser_id\x06;\x00Fi\b"
        let input: Vec<u8> = vec![
            4, 8, 123, 7, 73, 34, 16, 95, 99, 115, 114, 102, 95, 116, 111, 107, 101, 110, 6, 58, 6,
            69, 70, 73, 34, 49, 90, 116, 117, 76, 116, 49, 68, 89, 77, 103, 82, 74, 57, 101, 78,
            67, 118, 47, 66, 106, 107, 107, 54, 100, 74, 85, 54, 77, 69, 121, 119, 67, 88, 74, 97,
            87, 85, 97, 88, 119, 121, 84, 115, 61, 6, 59, 0, 70, 73, 34, 12, 117, 115, 101, 114,
            95, 105, 100, 6, 59, 0, 70, 105, 8,
        ];
        let result = decode(&input);
        // expected: {"_csrf_token"=>"ZtuLt1DYMgRJ9eNCv/Bjkk6dJU6MEywCXJaWUaXwyTs=", "user_id"=>3}
        println!("{:?}", result);
        panic!("foo");
    }

    #[test]
    fn test_string() {
        let input: Vec<u8> = vec![16, 95, 99, 115, 114, 102, 95, 116, 111, 107, 101, 110];
        let (rest, string) = decode_string(&input).unwrap();
        assert_eq!(string, "_csrf_token");
        println!("{:?}", rest);
        assert!(rest.is_empty());
    }
}
