#[derive(Clone, Debug, PartialEq, Eq)]
struct Element {
	name: String,
    attributes: Vec<(String, String)>,
    children: Vec<Element>,
}

type ParseResult<'a, Output> = Result<(&'a str, Output), &'a str>;

// this is what match_literal("a") essentially returns

fn the_letter_a(input: &str) -> ParseResult<()> {
    match input.chars().next() {
        Some('a') => Ok((&input['a'.len_utf8()..], ())),
        _ => Err(input),
    }
}

// this returns a closure!
//
// note the `impl` before the function return type and the `move` before the closure proper
//
// ironically, the variability of `expected` makes length extraction nicer to look at!

fn match_literal(expected: &'static str) -> impl Fn(&str) -> ParseResult<()> {
    move |input| match input.get(0..expected.len()) {
        Some(next) if next == expected => Ok((&input[expected.len()..], ())),
        _ => Err(input),
    }
}

// answer to Exercise 1
//
// see https://doc.rust-lang.org/std/primitive.str.html#method.strip_prefix

fn match_literal_improved(expected: &'static str) -> impl Fn(&str) -> ParseResult<()> {
    move |input| match input.strip_prefix(expected) {
        Some(next) => Ok((next, ())),
        None => Err(input),
    }
}

#[test]
fn literal_parser() {
    let parse = match_literal("abra");
    assert_eq!(Ok(("", ())), parse("abra"));
    assert_eq!(Ok(("kadabraalakazam", ())), parse("abrakadabraalakazam"));
    assert_eq!(Err(""), parse(""));
    assert_eq!(Err("abc"), parse("abc"));
    assert_eq!(Err("pikachu"), parse("pikachu"));
}

#[test]
fn literal_parser_improved() {
    let parse = match_literal_improved("abra");
    assert_eq!(Ok(("", ())), parse("abra"));
    assert_eq!(Ok(("kadabraalakazam", ())), parse("abrakadabraalakazam"));
    assert_eq!(Err(""), parse(""));
    assert_eq!(Err("abc"), parse("abc"));
    assert_eq!(Err("pikachu"), parse("pikachu"));
}

// matches the regex [a-zA-Z]([a-zA-Z0-9]|-)*

fn identifier(input: &str) -> ParseResult<String> {
    let mut matched = String::new();
    let mut chars = input.chars();

    match chars.next() {
        Some(next) if next.is_alphabetic() => matched.push(next),
        _ => return Err(input),
    }

    while let Some(next) = chars.next() {
        if next.is_alphanumeric() || next == '-' {
            matched.push(next);
        } else {
            break;
        }
    }

    let next_index = matched.len();
    Ok((&input[next_index..], matched))
}

// we need `.to_string()` since string literals are just slices

#[test]
fn identifier_parser() {
    assert_eq!(Ok(("", "a-b-c-d".to_string())), identifier("a-b-c-d"));
    assert_eq!(Ok((" b-c-d", "a".to_string())), identifier("a b-c-d"));
    assert_eq!(Err("!a-b-c-d"), identifier("!a-b-c-d"));
}

// given f and g, returns (f o g)

fn pair<P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Fn(&str) -> ParseResult<(R1, R2)>
where
    P1: Fn(&str) -> ParseResult<R1>,
    P2: Fn(&str) -> ParseResult<R2>,
{
    move |input| match parser1(input) {
        Ok((next_input, result1)) => match parser2(next_input) {
            Ok((final_input, result2)) => Ok((final_input, (result1, result2))),
            Err(err) => Err(err),
        },
        Err(err) => Err(err),
    }
}

// ｶｯｺｲｲ

#[test]
fn pair_combinator() {
    let tag_opener = pair(match_literal("<"), identifier);
    assert_eq!(Ok(("/>", ((), "br".to_string()))), tag_opener("<br/>"));
    assert_eq!(Err("oh no"), tag_opener("oh no"));
    assert_eq!(
        Err("!-- I'm just a comment! -->"),
        tag_opener("<!-- I'm just a comment! -->")
    );
}

fn map<P, F, A, B>(parser: P, map_fn: F) -> impl Fn(&str) -> ParseResult<B>
where
    P: Fn(&str) -> ParseResult<A>,
    F: Fn(A) -> B,
{
    move |input| parser(input).map(|(next_input, result)| (next_input, map_fn(result)))
}
