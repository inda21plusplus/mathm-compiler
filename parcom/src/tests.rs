use crate::{
    parsers::{CharParser, IntegerParser, StrParser, Ws},
    Error, Input, Parser, Span,
};

#[test]
fn test_char_parser() {
    let input = Input::new("nüll");
    let (input, output) = CharParser('n').parse(input).unwrap();
    assert_eq!(input.location, 1);
    assert_eq!(input.s, "üll");
    assert_eq!(output, Span::new(0..1));

    let (input, output) = CharParser('ü').parse(input).unwrap();
    assert_eq!(input.location, 3);
    assert_eq!(input.s, "ll");
    assert_eq!(output, Span::new(1..3));

    let err = CharParser('a').parse(input).unwrap_err();
    assert_eq!(err, Error::new(Span::new(3..4)));
}

#[test]
fn test_string_parser() {
    let input = Input::new("null");
    let (input, output) = StrParser("null").parse(input).unwrap();
    assert_eq!(input.location, 4);
    assert_eq!(input.s, "");
    assert_eq!(output, Span::new(0..4));

    let input = Input::new("nullable");
    let (rest, output) = StrParser("null").parse(input).unwrap();
    assert_eq!(rest.location, 4);
    assert_eq!(rest.s, "able");
    assert_eq!(output, Span::new(0..4));

    let input = Input::new("nil");
    let err = StrParser("null").parse(input).unwrap_err();
    assert_eq!(err, Error::new(Span::new(1..2)));
}

#[test]
fn test_whitespace_parser() {
    let input = Input::new("   \t \n kool");
    let (rest, span) = Ws.parse(input).unwrap();

    assert_eq!(rest.location, 7);
    assert_eq!(rest.s, "kool");
    assert_eq!(span, Some(Span::new(0..7)));

    let input = Input::new("asdf");
    let (rest, span) = Ws.parse(input).unwrap();

    assert_eq!(rest.location, 0);
    assert_eq!(rest.s, "asdf");
    assert_eq!(span, None);
}

#[test]
fn test_add_parser() {
    let input = Input::new("very true");
    let (rest, output) = (StrParser("very").c() + Ws + StrParser("true"))
        .parse(input)
        .unwrap();

    assert_eq!(rest.location, input.s.len());
    assert_eq!(rest.s, "");
    assert_eq!(
        output,
        ((Span::new(0..4), Some(Span::new(4..5))), Span::new(5..9))
    );

    let input = Input::new("verytrue");
    let (rest, output) = (StrParser("very").c() + StrParser("true"))
        .parse(input)
        .unwrap();

    assert_eq!(rest.location, 8);
    assert_eq!(rest.s, "");
    assert_eq!(output, (Span::new(0..4), Span::new(4..8)));
}

#[test]
fn test_shift_parser() {
    let input = Input::new("a b");
    let (rest, output) = ((CharParser('a').c() << Ws).c() + CharParser('b'))
        .parse(input)
        .unwrap();

    assert_eq!(rest.location, input.s.len());
    assert_eq!(rest.s, "");
    assert_eq!(output, (Span::new(0..1), Span::new(2..3)));

    let (rest, output) = (CharParser('a').c() + (Ws.c() >> CharParser('b')))
        .parse(input)
        .unwrap();

    assert_eq!(rest.location, input.s.len());
    assert_eq!(rest.s, "");
    assert_eq!(output, (Span::new(0..1), Span::new(2..3)));

    let input = Input::new("big boi!!");
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

    let parser = CharParser('c').map(|_| Cases::Char('c')).c()
        | StrParser("aoeu").map(|_| Cases::Str("aoeu"))
        | IntegerParser { base: 10 }.map(Cases::Int);

    let input = Input::new("c");
    let (rest, output) = parser.parse(input).unwrap();

    assert_eq!(rest.location, input.s.len());
    assert_eq!(rest.s, "");
    assert_eq!(output, Cases::Char('c'));

    let input = Input::new("aoeu");
    let (rest, output) = parser.parse(input).unwrap();

    assert_eq!(rest.location, input.s.len());
    assert_eq!(rest.s, "");
    assert_eq!(output, Cases::Str("aoeu"));

    let input = Input::new("69");
    let (rest, output) = parser.parse(input).unwrap();

    assert_eq!(rest.location, input.s.len());
    assert_eq!(rest.s, "");
    assert_eq!(output, Cases::Int(69));

    let input = Input::new("htns");
    let err = parser.parse(input).unwrap_err();

    assert_eq!(err, Error::new(Span::new(0..1)));
}

#[test]
fn test_integer_parser() {
    let input = Input::new("69420");
    let (rest, output) = IntegerParser { base: 10 }.parse(input).unwrap();

    assert_eq!(rest.location, input.s.len());
    assert_eq!(rest.s, "");
    assert_eq!(output, 69420);

    let input = Input::new("a, ");
    let (rest, output) = IntegerParser { base: 16 }.parse(input).unwrap();

    assert_eq!(rest.location, 1);
    assert_eq!(rest.s, ", ");
    assert_eq!(output, 10);

    let input = Input::new("deadbeef");
    let err = IntegerParser { base: 10 }.parse(input).unwrap_err();

    assert_eq!(err, Error::new(Span::new(0..1)));

    let input = Input::new("340282366920938463463374607431768211457");
    let err = IntegerParser { base: 10 }.parse(input).unwrap_err();

    assert_eq!(err, Error::new(Span::new(0..input.s.len())));
}

#[test]
fn test_optional_parser() {
    let parser = StrParser("hej").c() >> IntegerParser { base: 10 }.optional() << StrParser("då");

    let input = Input::new("hej123då");
    let (rest, output) = parser.parse(input).unwrap();

    assert_eq!(rest.location, input.s.len());
    assert_eq!(rest.s, "");
    assert_eq!(output, Some(123));

    let input = Input::new("hejdå");
    let (rest, output) = parser.parse(input).unwrap();

    assert_eq!(rest.location, input.s.len());
    assert_eq!(rest.s, "");
    assert_eq!(output, None);
}

#[test]
fn test_when_parser() {
    let input = Input::new("123");
    let (_, output) = (StrParser("123").when(false).map(|_| "str").c()
        | IntegerParser { base: 10 }.map(|_| "int"))
    .parse(input)
    .unwrap();

    assert_eq!(output, "int");

    let (_, output) = (StrParser("123").when(true).map(|_| "str").c()
        | IntegerParser { base: 10 }.map(|_| "int"))
    .parse(input)
    .unwrap();

    assert_eq!(output, "str");
}

#[test]
fn test_sep_by_parser() {
    let parser = CharParser('[').c()
        >> Ws
        >> IntegerParser { base: 10 }.sep_by(Ws.c() + CharParser(',') + Ws)
        << Ws
        << CharParser(']');
    let input = Input::new("[1, 2, 3, 4, 5]");
    let (rest, output) = parser.parse(input).unwrap();

    assert_eq!(rest.location, input.s.len());
    assert_eq!(rest.s, "");
    assert_eq!(output, vec![1, 2, 3, 4, 5]);

    let input = Input::new("[]");
    let (rest, output) = parser.parse(input).unwrap();

    assert_eq!(rest.location, input.s.len());
    assert_eq!(rest.s, "");
    assert_eq!(output, []);
}
