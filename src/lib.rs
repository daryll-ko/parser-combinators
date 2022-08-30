#[derive(Clone, Debug, PartialEq, Eq)]
struct Element {
    name: String,
    attributes: Vec<(String, String)>,
    children: Vec<Element>,
}

type ParseResult<'a, Output> = Result<(&'a str, Output), &'a str>;

trait Parser<'a, Output> {
    fn parse(&self, input: &'a str) -> ParseResult<'a, Output>;

    fn map<F, NewOutput>(self, map_fn: F) -> BoxedParser<'a, NewOutput>
    where
        Self: Sized + 'a,
        Output: 'a,
        NewOutput: 'a,
        F: Fn(Output) -> NewOutput + 'a,
    {
        BoxedParser::new(map(self, map_fn))
    }

    fn pred<F>(self, pred_fn: F) -> BoxedParser<'a, Output>
    where
        Self: Sized + 'a,
        Output: 'a,
        F: Fn(&Output) -> bool + 'a,
    {
        BoxedParser::new(pred(self, pred_fn))
    }

    fn and_then<F, NextParser, NewOutput>(self, f: F) -> BoxedParser<'a, NewOutput>
    where
        Self: Sized + 'a,
        Output: 'a,
        NewOutput: 'a,
        NextParser: Parser<'a, NewOutput> + 'a,
        F: Fn(Output) -> NextParser + 'a,
    {
        BoxedParser::new(and_then(self, f))
    }
}

impl<'a, F, Output> Parser<'a, Output> for F
where
    F: Fn(&'a str) -> ParseResult<Output>,
{
    fn parse(&self, input: &'a str) -> ParseResult<'a, Output> {
        self(input)
    }
}

struct BoxedParser<'a, Output> {
    parser: Box<dyn Parser<'a, Output> + 'a>,
}

impl<'a, Output> BoxedParser<'a, Output> {
    fn new<P>(parser: P) -> Self
    where
        P: Parser<'a, Output> + 'a,
    {
        BoxedParser {
            parser: Box::new(parser),
        }
    }
}

impl<'a, Output> Parser<'a, Output> for BoxedParser<'a, Output> {
    fn parse(&self, input: &'a str) -> ParseResult<'a, Output> {
        self.parser.parse(input)
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

// answer to exercise
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

#[test]
fn left_combinator() {
    let tag_closer = left(identifier, match_literal("/>"));
    assert_eq!(Ok(("", "br".to_string())), tag_closer.parse("br/>"));
    assert_eq!(Err(" no"), tag_closer.parse("oh no"));
    assert_eq!(
        Err("<!-- I'm just a comment! -->"),
        tag_closer.parse("<!-- I'm just a comment! -->")
    );
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

fn one_or_more<'a, P, A>(parser: P) -> impl Parser<'a, Vec<A>>
where
    P: Parser<'a, A>,
{
    move |mut input| {
        let mut result = Vec::new();

        if let Ok((next_input, first_item)) = parser.parse(input) {
            input = next_input;
            result.push(first_item);
        } else {
            return Err(input);
        }

        while let Ok((next_input, next_item)) = parser.parse(input) {
            input = next_input;
            result.push(next_item);
        }

        Ok((input, result))
    }
}

#[test]
fn one_or_more_combinator() {
    let parser = one_or_more(match_literal("le"));
    assert_eq!(Ok(("", vec![(), (), ()])), parser.parse("lelele"));
    assert_eq!(
        Err("delelelelelewhooop"),
        parser.parse("delelelelelewhooop")
    );
    assert_eq!(Err(""), parser.parse(""));
}

fn zero_or_more<'a, P, A>(parser: P) -> impl Parser<'a, Vec<A>>
where
    P: Parser<'a, A>,
{
    move |mut input| {
        let mut result = Vec::new();

        while let Ok((next_input, next_item)) = parser.parse(input) {
            input = next_input;
            result.push(next_item);
        }

        Ok((input, result))
    }
}

#[test]
fn zero_or_more_combinator() {
    let parser = zero_or_more(match_literal("le"));
    assert_eq!(Ok(("", vec![(), (), ()])), parser.parse("lelele"));
    assert_eq!(
        Ok(("delelelelelewhooop", vec![])),
        parser.parse("delelelelelewhooop")
    );
    assert_eq!(Ok(("", vec![])), parser.parse(""));
}

fn any_char(input: &str) -> ParseResult<char> {
    match input.chars().next() {
        Some(next) => Ok((&input[next.len_utf8()..], next)),
        _ => Err(input),
    }
}

fn pred<'a, P, A, F>(parser: P, predicate: F) -> impl Parser<'a, A>
where
    P: Parser<'a, A>,
    F: Fn(&A) -> bool,
{
    move |input| {
        if let Ok((next_input, value)) = parser.parse(input) {
            if predicate(&value) {
                return Ok((next_input, value));
            }
        }
        Err(input)
    }
}

#[test]
fn predicate_combinator() {
    let parser = pred(any_char, |c| *c == 'o');
    assert_eq!(Ok(("ctazooka", 'o')), parser.parse("octazooka"));
    assert_eq!(Err("bazooka"), parser.parse("bazooka"));
}

fn whitespace_char<'a>() -> impl Parser<'a, char> {
    pred(any_char, |c| c.is_whitespace())
}

fn space1<'a>() -> impl Parser<'a, Vec<char>> {
    one_or_more(whitespace_char())
}

fn space0<'a>() -> impl Parser<'a, Vec<char>> {
    zero_or_more(whitespace_char())
}

fn quoted_string<'a>() -> impl Parser<'a, String> {
    right(
        match_literal("\""),
        left(
            zero_or_more(any_char.pred(|c| *c != '"')),
            match_literal("\""),
        ),
    )
    .map(|chars| chars.into_iter().collect())
}

#[test]
fn quoted_string_parser() {
    assert_eq!(
        Ok(("", "value".to_string())),
        quoted_string().parse("\"value\"")
    );
}

fn attribute_pair<'a>() -> impl Parser<'a, (String, String)> {
    pair(identifier, right(match_literal("="), quoted_string()))
}

fn attributes<'a>() -> impl Parser<'a, Vec<(String, String)>> {
    zero_or_more(right(space1(), attribute_pair()))
}

#[test]
fn attribute_parser() {
    assert_eq!(
        Ok((
            "",
            vec![
                ("one".to_string(), "1".to_string()),
                ("two".to_string(), "2".to_string())
            ]
        )),
        attributes().parse(" one=\"1\" two=\"2\"")
    );
}

fn element_start<'a>() -> impl Parser<'a, (String, Vec<(String, String)>)> {
    right(match_literal("<"), pair(identifier, attributes()))
}

fn single_element<'a>() -> impl Parser<'a, Element> {
    left(element_start(), match_literal("/>")).map(|(name, attributes)| Element {
        name,
        attributes,
        children: vec![],
    })
}

#[test]
fn single_element_parser() {
    assert_eq!(
        Ok((
            "",
            Element {
                name: "div".to_string(),
                attributes: vec![("class".to_string(), "container".to_string())],
                children: vec![],
            }
        )),
        single_element().parse("<div class=\"container\"/>")
    )
}

fn open_element<'a>() -> impl Parser<'a, Element> {
    left(element_start(), match_literal(">")).map(|(name, attributes)| Element {
        name,
        attributes,
        children: vec![],
    })
}

fn either<'a, P1, P2, A>(parser1: P1, parser2: P2) -> impl Parser<'a, A>
where
    P1: Parser<'a, A>,
    P2: Parser<'a, A>,
{
    move |input| match parser1.parse(input) {
        ok @ Ok(_) => ok,
        Err(_) => parser2.parse(input),
    }
}

fn whitespace_wrap<'a, P, A>(parser: P) -> impl Parser<'a, A>
where
    P: Parser<'a, A>,
{
    right(space0(), left(parser, space0()))
}

fn element<'a>() -> impl Parser<'a, Element> {
    whitespace_wrap(either(single_element(), parent_element()))
}

fn close_element<'a>(expected_name: String) -> impl Parser<'a, String> {
    right(match_literal("</"), left(identifier, match_literal(">")))
        .pred(move |name| name == &expected_name)
}

fn parent_element<'a>() -> impl Parser<'a, Element> {
    open_element().and_then(|el| {
        left(zero_or_more(element()), close_element(el.name.clone())).map(move |children| {
            let mut el = el.clone();
            el.children = children;
            el
        })
    })
}

fn and_then<'a, P, F, A, B, NextP>(parser: P, f: F) -> impl Parser<'a, B>
where
    P: Parser<'a, A>,
    NextP: Parser<'a, B>,
    F: Fn(A) -> NextP,
{
    move |input| match parser.parse(input) {
        Ok((next_input, result)) => f(result).parse(next_input),
        Err(err) => Err(err),
    }
}

#[test]
fn xml_parser() {
    let doc = r#"
		<div class="wrapper">
			<p id="intro"></p>
			<br/>
			<div>
				<button class="start" onclick="click()"></button>
			</div>
		</div>"#;
    let parsed_doc = Element {
        name: "div".to_string(),
        attributes: vec![("class".to_string(), "wrapper".to_string())],
        children: vec![
            Element {
                name: "p".to_string(),
                attributes: vec![("id".to_string(), "intro".to_string())],
                children: vec![],
            },
            Element {
                name: "br".to_string(),
                attributes: vec![],
                children: vec![],
            },
            Element {
                name: "div".to_string(),
                attributes: vec![],
                children: vec![Element {
                    name: "button".to_string(),
                    attributes: vec![
                        ("class".to_string(), "start".to_string()),
                        ("onclick".to_string(), "click()".to_string()),
                    ],
                    children: vec![],
                }],
            },
        ],
    };
    assert_eq!(Ok(("", parsed_doc)), element().parse(doc));
}

#[test]
fn oh_no() {
    let doc = r#"
		<div>
			<html/>
		</span>"#;
    assert_eq!(Err("</span>"), element().parse(doc));
}
