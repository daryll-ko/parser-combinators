#[derive(Clone, Debug, PartialEq, Eq)]
struct Element {
    name: String,
    attributes: Vec<(String, String)>,
    children: Vec<Element>,
}

type ParseResult<'a, Output> = Result<(&'a str, Output), &'a str>;

trait Parser<'a, Output> {
    fn parse(&self, input: &'a str) -> ParseResult<'a, Output>;
}

impl<'a, F, Output> Parser<'a, Output> for F
where
    F: Fn(&'a str) -> ParseResult<Output>,
{
    fn parse(&self, input: &'a str) -> ParseResult<'a, Output> {
        self(input)
    }
}

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

fn match_literal<'a>(expected: &'static str) -> impl Parser<'a, ()> {
    move |input: &'a str| match input.get(0..expected.len()) {
        Some(next) if next == expected => Ok((&input[expected.len()..], ())),
        _ => Err(input),
    }
}

// answer to Exercise 1
//
// see https://doc.rust-lang.org/std/primitive.str.html#method.strip_prefix

fn match_literal_improved<'a>(expected: &'static str) -> impl Parser<'a, ()> {
    move |input: &'a str| match input.strip_prefix(expected) {
        Some(next) => Ok((next, ())),
        None => Err(input),
    }
}

#[test]
fn literal_parser() {
    let parser = match_literal("abra");
    assert_eq!(Ok(("", ())), parser.parse("abra"));
    assert_eq!(
        Ok(("kadabraalakazam", ())),
        parser.parse("abrakadabraalakazam")
    );
    assert_eq!(Err(""), parser.parse(""));
    assert_eq!(Err("abc"), parser.parse("abc"));
    assert_eq!(Err("pikachu"), parser.parse("pikachu"));
}

#[test]
fn literal_parser_improved() {
    let parser = match_literal_improved("abra");
    assert_eq!(Ok(("", ())), parser.parse("abra"));
    assert_eq!(
        Ok(("kadabraalakazam", ())),
        parser.parse("abrakadabraalakazam")
    );
    assert_eq!(Err(""), parser.parse(""));
    assert_eq!(Err("abc"), parser.parse("abc"));
    assert_eq!(Err("pikachu"), parser.parse("pikachu"));
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

fn pair<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, (R1, R2)>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    move |input| {
        parser1.parse(input).and_then(|(next_input, result1)| {
            parser2
                .parse(next_input)
                .map(|(last_input, result2)| (last_input, (result1, result2)))
        })
    }
}

// ｶｯｺｲｲ

#[test]
fn pair_combinator() {
    let tag_opener = pair(match_literal("<"), identifier);
    assert_eq!(
        Ok(("/>", ((), "br".to_string()))),
        tag_opener.parse("<br/>")
    );
    assert_eq!(Err("oh no"), tag_opener.parse("oh no"));
    assert_eq!(
        Err("!-- I'm just a comment! -->"),
        tag_opener.parse("<!-- I'm just a comment! -->")
    );
}

fn map<'a, P, F, A, B>(parser: P, map_fn: F) -> impl Parser<'a, B>
where
    P: Parser<'a, A>,
    F: Fn(A) -> B,
{
    move |input| {
        parser
            .parse(input)
            .map(|(next_input, result)| (next_input, map_fn(result)))
    }
}

fn left<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, R1>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    map(pair(parser1, parser2), |(left, _right)| left)
}

fn right<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, R2>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    map(pair(parser1, parser2), |(_left, right)| right)
}

#[test]
fn right_combinator() {
    let tag_opener = right(match_literal("<"), identifier);
    assert_eq!(Ok(("/>", "br".to_string())), tag_opener.parse("<br/>"));
    assert_eq!(Err("oh no"), tag_opener.parse("oh no"));
    assert_eq!(
        Err("!-- I'm just a comment! -->"),
        tag_opener.parse("<!-- I'm just a comment! -->")
    );
}
