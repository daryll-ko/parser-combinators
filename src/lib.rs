#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct Element {
	name: String,
	attributes: Vec<(String, String)>,
	children: Vec<Element>,
}

fn the_letter_a(input: &str) -> Result<(&str, ()), &str> {
	match input.chars().next() {
		Some('a') => Ok((&input['a'.len_utf8()..], ())),
		_ => Err(input)
	}
}