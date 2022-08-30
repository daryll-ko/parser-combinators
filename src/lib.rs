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

