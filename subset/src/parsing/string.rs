use parcom::{parsers::CharParser, Error, Input, Parser, Span};

#[derive(Debug, Clone)]
pub struct StringLiteral {
    pub span: Span,
    pub value: String,
}

#[derive(Debug, Clone, Copy)]
pub struct StringLiteralParser;

impl StringLiteralParser {
    fn unescape_char(c: char) -> Option<char> {
        match c {
            '"' => Some('"'),
            '\\' => Some('\\'),
            'n' => Some('\n'),
            't' => Some('\t'),
            'r' => Some('\r'),
            _ => None,
        }
    }
}

impl Parser for StringLiteralParser {
    type Output = StringLiteral;

    // todo: make more functional or something. refactor it somehow
    // todo: support r"\"
    // todo: support r_" " "_
    // todo: support c"null-terminated, and maybe inoperable with c-style strings or whatever"
    fn parse<'i>(self, input: Input<'i>) -> Result<(Input<'i>, Self::Output), Error> {
        let start = input.location;
        let (mut input, _) = CharParser('"').parse(input)?;

        let mut value = String::new();

        loop {
            let c = match input.next() {
                None => return Err(Error::new(Span::new(input.location..input.location))),
                Some((rest, ch)) => {
                    input = rest;
                    ch
                }
            };

            match c {
                '"' => break,
                '\\' => match input.next() {
                    Some((rest, c)) => {
                        input = rest;
                        match Self::unescape_char(c) {
                            Some(c) => value.push(c),
                            None => return Err(Error::new(Span::first(&input))),
                        }
                    }
                    _ => return Err(Error::new(Span::first(&input))),
                },
                c => value.push(c),
            }
        }

        Ok((
            input,
            StringLiteral {
                span: Span::new(start..input.location),
                value,
            },
        ))
    }
}
