use crate::{
    error::ParsingError,
    parser::{ws, BoolParser, CharParser, Either, Input, IntegerParser, Parser, Span, StrParser},
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
    assert_eq!(err, ParsingError::new(Span::single(2)));
}

#[test]
fn test_string_parser() {
    let input = Input::from_str("null");
    let (input, output) = StrParser("null").parse(input).unwrap();
    assert_eq!(input.location, 4);
    assert_eq!(input.s, "");
    assert_eq!(output, "null");

    let input = Input::from_str("nullable");
    let (rest, output) = StrParser("null").parse(input).unwrap();
    assert_eq!(rest.location, 4);
    assert_eq!(rest.s, "able");
    assert_eq!(output, "null");

    let input = Input::from_str("nil");
    let err = StrParser("null").parse(input).unwrap_err();
    assert_eq!(err, ParsingError::new(Span::single(1)));
}

#[test]
fn test_bool_parser() {
    let input = Input::from_str("true");
    let (rest, output) = BoolParser.parse(input).unwrap();
    assert_eq!(rest.location, 4);
    assert_eq!(rest.s, "");
    assert_eq!(output, true);

    let input = Input::from_str("false indeed");
    let (rest, output) = BoolParser.parse(input).unwrap();
    assert_eq!(rest.location, 5);
    assert_eq!(rest.s, " indeed");
    assert_eq!(output, false);

    let input = Input::from_str("truish");
    let err = BoolParser.parse(input).unwrap_err();
    assert_eq!(err, ParsingError::new(Span::single(3)));
}

#[test]
fn test_whitespace_parser() {
    let input = Input::from_str("   \t \n kool");
    let (rest, span) = ws().parse(input).unwrap();

    assert_eq!(rest.location, 7);
    assert_eq!(rest.s, "kool");
    assert_eq!(span, Span::new(0..7));
}

#[test]
fn test_add_parser() {
    let input = Input::from_str("true false");
    let (rest, output) = (BoolParser.c() + ws() + BoolParser).parse(input).unwrap();

    assert_eq!(rest.location, 10);
    assert_eq!(rest.s, "");
    assert_eq!(output, ((true, Span::single(4)), false));

    let input = Input::from_str("verytrue");
    let (rest, output) = (StrParser("very").c() + BoolParser).parse(input).unwrap();

    assert_eq!(rest.location, 8);
    assert_eq!(rest.s, "");
    assert_eq!(output, ("very", true));
}

#[test]
fn test_shift_parser() {
    let input = Input::from_str("true false");
    let (rest, output) = ((BoolParser.c() << ws()).c() + BoolParser)
        .parse(input)
        .unwrap();

    assert_eq!(rest.location, 10);
    assert_eq!(rest.s, "");
    assert_eq!(output, (true, false));

    let input = Input::from_str("big boi!!");
    let (rest, output) = (StrParser("big").c() >> ws() >> StrParser("boi") << StrParser("!"))
        .parse(input)
        .unwrap();

    assert_eq!(rest.location, 8);
    assert_eq!(rest.s, "!");
    assert_eq!(output, "boi");
}

#[test]
fn test_either_parser() {
    let input = Input::from_str("true");
    let (rest, output) = (BoolParser.c() / StrParser("aoeu")).parse(input).unwrap();

    assert_eq!(rest.location, input.s.len());
    assert_eq!(rest.s, "");
    assert_eq!(output, Either::L(true));

    let input = Input::from_str("aoeu");
    let (rest, output) = (BoolParser.c() / StrParser("aoeu")).parse(input).unwrap();

    assert_eq!(rest.location, input.s.len());
    assert_eq!(rest.s, "");
    assert_eq!(output, Either::R("aoeu"));

    let input = Input::from_str("htns");
    let err = (BoolParser.c() / StrParser("aoeu"))
        .parse(input)
        .unwrap_err();

    assert_eq!(err, ParsingError::new(Span::single(0)));
}

#[test]
fn test_integer_parser() {
    let input = Input::from_str("69420");
    let (rest, output) = IntegerParser.parse(input).unwrap();

    assert_eq!(rest.location, input.s.len());
    assert_eq!(rest.s, "");
    assert_eq!(output, 69420);

    let input = Input::from_str("420, ");
    let (rest, output) = IntegerParser.parse(input).unwrap();

    assert_eq!(rest.location, 3);
    assert_eq!(rest.s, ", ");
    assert_eq!(output, 420);

    let input = Input::from_str("x");
    let err = IntegerParser.parse(input).unwrap_err();

    assert_eq!(err, ParsingError::new(Span::single(0)));
}
