#[derive(Clone, Debug, PartialEq, Eq)]
struct Element {
	name: String,
    attributes: Vec<(String, String)>,
    children: Vec<Element>,
}

// this is what match_literal("a") essentially returns

fn the_letter_a(input: &str) -> Result<(&str, ()), &str> {
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

fn match_literal(expected: &'static str) -> impl Fn(&str) -> Result<(&str, ()), &str> {
	move |input| match input.get(0..expected.len()) {
		Some (next) if next == expected => Ok((&input[expected.len()..], ())),
		_ => Err(input),
	}
}

// answer to Exercise 1
//
// see https://doc.rust-lang.org/std/primitive.str.html#method.strip_prefix

fn match_literal_improved(expected: &'static str) -> impl Fn(&str) -> Result<(&str, ()), &str> {
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

fn identifier(input: &str) -> Result<(&str, String), &str> {
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