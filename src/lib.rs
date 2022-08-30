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

#[test]
fn literal_parser() {
	let parse = match_literal("abra");
	assert_eq!(Ok(("", ())), parse("abra"));
	assert_eq!(Ok(("kadabraalakazam", ())), parse("abrakadabraalakazam"));
	assert_eq!(Err(""), parse(""));
	assert_eq!(Err("pikachu"), parse("pikachu"));
}
