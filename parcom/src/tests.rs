use crate::{
    parsers::{CharParser, IntegerParser, StrParser, Ws},
    Error, Input, Parser, Span,
};

#[test]
fn test_char_parser() {
    let input = Input::from_str("null");
    let (input, output) = CharParser('n').parse(input).unwrap();
    assert_eq!(input.location, 1);
    assert_eq!(input.s, "ull");
    assert_eq!(output, 'n');

    let (input, output) = CharParser('u').parse(input).unwrap();
    assert_eq!(input.location, 2);
    assert_eq!(input.s, "ll");
    assert_eq!(output, 'u');

    let err = CharParser('a').parse(input).unwrap_err();
    assert_eq!(err, Error::new(Span::new(2..3)));
}

#[test]
fn test_string_parser() {
    let input = Input::from_str("null");
    let (input, output) = StrParser("null").parse(input).unwrap();
    assert_eq!(input.location, 4);
    assert_eq!(input.s, "");
    assert_eq!(output, Span::new(0..4));

    let input = Input::from_str("nullable");
    let (rest, output) = StrParser("null").parse(input).unwrap();
    assert_eq!(rest.location, 4);
    assert_eq!(rest.s, "able");
    assert_eq!(output, Span::new(0..4));

    let input = Input::from_str("nil");
    let err = StrParser("null").parse(input).unwrap_err();
    assert_eq!(err, Error::new(Span::new(1..2)));
}

#[test]
fn test_whitespace_parser() {
    let input = Input::from_str("   \t \n kool");
    let (rest, span) = Ws.parse(input).unwrap();

    assert_eq!(rest.location, 7);
    assert_eq!(rest.s, "kool");
    assert_eq!(span, Span::new(0..7));
}

#[test]
fn test_add_parser() {
    let input = Input::from_str("very true");
    let (rest, output) = (StrParser("very").c() + Ws + StrParser("true"))
        .parse(input)
        .unwrap();

    assert_eq!(rest.location, input.s.len());
    assert_eq!(rest.s, "");
    assert_eq!(
        output,
        ((Span::new(0..4), Span::new(4..5)), Span::new(5..9))
    );

    let input = Input::from_str("verytrue");
    let (rest, output) = (StrParser("very").c() + StrParser("true"))
        .parse(input)
        .unwrap();

    assert_eq!(rest.location, 8);
    assert_eq!(rest.s, "");
    assert_eq!(output, (Span::new(0..4), Span::new(4..8)));
}

#[test]
fn test_shift_parser() {
    let input = Input::from_str("a b");
    let (rest, output) = ((CharParser('a').c() << Ws).c() + CharParser('b'))
        .parse(input)
        .unwrap();

    assert_eq!(rest.location, input.s.len());
    assert_eq!(rest.s, "");
    assert_eq!(output, ('a', 'b'));

    let (rest, output) = (CharParser('a').c() + (Ws.c() >> CharParser('b')))
        .parse(input)
        .unwrap();

    assert_eq!(rest.location, input.s.len());
    assert_eq!(rest.s, "");
    assert_eq!(output, ('a', 'b'));

    let input = Input::from_str("big boi!!");
    let (rest, output) = (StrParser("big").c() >> Ws >> StrParser("boi") << StrParser("!"))
        .parse(input)
        .unwrap();

    assert_eq!(rest.location, 8);
    assert_eq!(rest.s, "!");
    assert_eq!(output, Span::new(4..7));
}

#[test]
fn test_or_parser() {
    #[derive(Debug, PartialEq, Eq, Clone, Copy)]
    enum Cases<'s> {
        Char(char),
        Str(&'s str),
        Int(u128),
    }

    let parser = CharParser('c').map(Cases::Char).c()
        | StrParser("aoeu").map(|_| Cases::Str("aoeu"))
        | IntegerParser { base: 10 }.map(Cases::Int);

    let input = Input::from_str("c");
    let (rest, output) = parser.parse(input).unwrap();

    assert_eq!(rest.location, input.s.len());
    assert_eq!(rest.s, "");
    assert_eq!(output, Cases::Char('c'));

    let input = Input::from_str("aoeu");
    let (rest, output) = parser.parse(input).unwrap();

    assert_eq!(rest.location, input.s.len());
    assert_eq!(rest.s, "");
    assert_eq!(output, Cases::Str("aoeu"));

    let input = Input::from_str("69");
    let (rest, output) = parser.parse(input).unwrap();

    assert_eq!(rest.location, input.s.len());
    assert_eq!(rest.s, "");
    assert_eq!(output, Cases::Int(69));

    let input = Input::from_str("htns");
    let err = parser.parse(input).unwrap_err();

    assert_eq!(err, Error::new(Span::new(0..1)));
}

#[test]
fn test_integer_parser() {
    let input = Input::from_str("69420");
    let (rest, output) = IntegerParser { base: 10 }.parse(input).unwrap();

    assert_eq!(rest.location, input.s.len());
    assert_eq!(rest.s, "");
    assert_eq!(output, 69420);

    let input = Input::from_str("a, ");
    let (rest, output) = IntegerParser { base: 16 }.parse(input).unwrap();

    assert_eq!(rest.location, 1);
    assert_eq!(rest.s, ", ");
    assert_eq!(output, 10);

    let input = Input::from_str("deadbeef");
    let err = IntegerParser { base: 10 }.parse(input).unwrap_err();

    assert_eq!(err, Error::new(Span::new(0..1)));

    let input = Input::from_str("340282366920938463463374607431768211457");
    let err = IntegerParser { base: 10 }.parse(input).unwrap_err();

    assert_eq!(err, Error::new(Span::new(0..input.s.len())));
}

#[test]
fn test_optional_parser() {
    let parser = StrParser("hej").c() >> IntegerParser { base: 10 }.optional() << StrParser("då");

    let input = Input::from_str("hej123då");
    let (rest, output) = parser.parse(input).unwrap();

    assert_eq!(rest.location, input.s.len());
    assert_eq!(rest.s, "");
    assert_eq!(output, Some(123));

    let input = Input::from_str("hejdå");
    let (rest, output) = parser.parse(input).unwrap();

    assert_eq!(rest.location, input.s.len());
    assert_eq!(rest.s, "");
    assert_eq!(output, None);
}
