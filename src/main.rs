use std::{collections::HashMap, fs};

#[derive(Debug, Clone)]
pub enum Number {
    Float(f64),
    Integer(i64),
}

impl std::fmt::Display for Number {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Number::Float(fl) => write!(f, "{}", fl),
            Number::Integer(i) => write!(f, "{}", i),
        }
    }
}

#[derive(Debug)]
pub enum JsonObject {
    Array(Vec<JsonObject>),
    Bool(bool),
    Null,
    Number(Number),
    Object(HashMap<String, JsonObject>),
    String(String),
}

#[derive(Debug, Clone)]
pub enum Token {
    LeftBrace,
    LeftBracket,
    Number(Number),
    RightBrace,
    RightBracket,
    String(String),
    Colon,
    Comma,
    Bool(bool),
    Null,
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::LeftBrace => write!(f, "LeftBrace"),
            Token::LeftBracket => write!(f, "LeftBracket"),
            Token::Number(num) => write!(f, "{}", num),
            Token::RightBrace => write!(f, "RightBrace"),
            Token::RightBracket => write!(f, "RightBracket"),
            Token::String(s) => write!(f, "String({})", s),
            Token::Colon => write!(f, "Colon"),
            Token::Comma => write!(f, "Comma"),
            Token::Bool(b) => write!(f, "Bool({})", b),
            Token::Null => write!(f, "Null"),
        }
    }
}

fn skip_whitespace(data: &[char]) -> &[char] {
    let mut i = 0;
    while i < data.len() && data[i].is_whitespace() {
        i += 1;
    }
    &data[i..]
}

fn tokenize(data: &[char]) -> anyhow::Result<Vec<Token>> {
    let mut tokens = Vec::new();
    let mut rest = skip_whitespace(data);

    while !rest.is_empty() {
        rest = skip_whitespace(rest);
        if rest.is_empty() {
            break;
        }

        match rest[0] {
            '{' => {
                tokens.push(Token::LeftBrace);
                rest = &rest[1..];
            }
            '}' => {
                tokens.push(Token::RightBrace);
                rest = &rest[1..];
            }
            '[' => {
                tokens.push(Token::LeftBracket);
                rest = &rest[1..];
            }
            ']' => {
                tokens.push(Token::RightBracket);
                rest = &rest[1..];
            }
            ':' => {
                tokens.push(Token::Colon);
                rest = &rest[1..];
            }
            ',' => {
                tokens.push(Token::Comma);
                rest = &rest[1..];
            }
            '"' => {
                let (s, remaining) = tokenize_string(rest)?;
                tokens.push(Token::String(s));
                rest = remaining;
            }
            't' | 'f' => {
                let (b, remaining) = tokenize_bool(rest)?;
                tokens.push(Token::Bool(b));
                rest = remaining;
            }
            'n' => {
                let remaining = tokenize_null(rest)?;
                tokens.push(Token::Null);
                rest = remaining;
            }
            '-' | '0'..='9' => {
                let (num, remaining) = tokenize_number(rest)?;
                tokens.push(Token::Number(num));
                rest = remaining;
            }
            _ => return Err(anyhow::anyhow!("Unexpected character: {}", rest[0])),
        }
    }

    Ok(tokens)
}

fn tokenize_string(data: &[char]) -> anyhow::Result<(String, &[char])> {
    let mut s = String::new();
    let mut i = 1;

    while i < data.len() && data[i] != '"' {
        s.push(data[i]);
        i += 1;
    }

    if i >= data.len() {
        return Err(anyhow::anyhow!("Unterminated string"));
    }

    Ok((s, &data[i + 1..]))
}

fn tokenize_number(data: &[char]) -> anyhow::Result<(Number, &[char])> {
    let mut s = String::new();
    let mut i = 0;
    let mut is_float = false;

    if data[i] == '-' {
        s.push(data[i]);
        i += 1;
    }

    while i < data.len() && (data[i].is_ascii_digit() || data[i] == '.') {
        if data[i] == '.' {
            is_float = true;
        }
        s.push(data[i]);
        i += 1;
    }

    let num = if is_float {
        Number::Float(s.parse()?)
    } else {
        Number::Integer(s.parse()?)
    };

    Ok((num, &data[i..]))
}

fn tokenize_bool(data: &[char]) -> anyhow::Result<(bool, &[char])> {
    if data.len() >= 4 && data[0..4] == ['t', 'r', 'u', 'e'] {
        Ok((true, &data[4..]))
    } else if data.len() >= 5 && data[0..5] == ['f', 'a', 'l', 's', 'e'] {
        Ok((false, &data[5..]))
    } else {
        Err(anyhow::anyhow!("Invalid boolean"))
    }
}

fn tokenize_null(data: &[char]) -> anyhow::Result<&[char]> {
    if data.len() >= 4 && data[0..4] == ['n', 'u', 'l', 'l'] {
        Ok(&data[4..])
    } else {
        Err(anyhow::anyhow!("Invalid null"))
    }
}

fn parse_json_array(tokens: &[Token], pos: &mut usize) -> anyhow::Result<JsonObject> {
    let mut arr = Vec::new();
    *pos += 1; // skip '['

    while *pos < tokens.len() {
        match &tokens[*pos] {
            Token::RightBracket => {
                *pos += 1;
                return Ok(JsonObject::Array(arr));
            }
            Token::Comma => {
                *pos += 1;
            }
            _ => {
                let val = parse_value(tokens, pos)?;
                arr.push(val);
            }
        }
    }

    Err(anyhow::anyhow!("Unterminated array"))
}

fn parse_json_object(tokens: &[Token], pos: &mut usize) -> anyhow::Result<JsonObject> {
    let mut obj = HashMap::new();
    *pos += 1; // skip '{'

    while *pos < tokens.len() {
        match &tokens[*pos] {
            Token::RightBrace => {
                *pos += 1;
                return Ok(JsonObject::Object(obj));
            }
            Token::Comma => {
                *pos += 1;
            }
            Token::String(key) => {
                let key = key.clone();
                *pos += 1;

                if *pos >= tokens.len() || !matches!(tokens[*pos], Token::Colon) {
                    return Err(anyhow::anyhow!("Expected colon after key"));
                }
                *pos += 1;

                let val = parse_value(tokens, pos)?;
                obj.insert(key, val);
            }
            _ => return Err(anyhow::anyhow!("Expected string key in object")),
        }
    }

    Err(anyhow::anyhow!("Unterminated object"))
}

fn parse_value(tokens: &[Token], pos: &mut usize) -> anyhow::Result<JsonObject> {
    if *pos >= tokens.len() {
        return Err(anyhow::anyhow!("Unexpected end of tokens"));
    }

    match &tokens[*pos] {
        Token::LeftBrace => parse_json_object(tokens, pos),
        Token::LeftBracket => parse_json_array(tokens, pos),
        Token::String(s) => {
            *pos += 1;
            Ok(JsonObject::String(s.clone()))
        }
        Token::Number(n) => {
            *pos += 1;
            Ok(JsonObject::Number(n.clone()))
        }
        Token::Bool(b) => {
            *pos += 1;
            Ok(JsonObject::Bool(*b))
        }
        Token::Null => {
            *pos += 1;
            Ok(JsonObject::Null)
        }
        _ => Err(anyhow::anyhow!("Unexpected token: {}", tokens[*pos])),
    }
}

fn parse_object(data: &[char]) -> anyhow::Result<JsonObject> {
    let tokens = tokenize(data)?;

    if tokens.is_empty() {
        return Err(anyhow::anyhow!("Empty input"));
    }

    let mut pos = 0;
    parse_value(&tokens, &mut pos)
}

fn main() -> anyhow::Result<()> {
    let args: Vec<String> = std::env::args().skip(1).collect();
    if args.is_empty() {
        return Err(anyhow::anyhow!("Missing args!"));
    }
    let content = fs::read_to_string(&args[0])?;
    let chars: Vec<char> = content.chars().collect();
    let json = parse_object(&chars)?;
    println!("{json:#?}");

    Ok(())
}
