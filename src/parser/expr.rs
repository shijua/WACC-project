use crate::ast::Expr;
use chumsky::error::Rich;
use chumsky::prelude::{any, choice, just, none_of, one_of, SimpleSpan};
use chumsky::{extra, text, IterParser, Parser};

fn is_valid_single_ascii(chr: u8) -> bool {
    (chr >= 0x20 && chr < 0x80) && (chr != b'\'') && (chr != b'\"') && (chr != b'\\')
}

pub type Span = SimpleSpan<usize>;

pub type Spanned<T> = (T, Span);

fn expr_atom_literal<'a>(
) -> impl Parser<'a, &'a str, Spanned<Expr>, extra::Err<Rich<'a, char, Span>>> {
    // <int-liter> ::= ⟨'+' | '-'⟩? ⟨digit⟩+
    let int_liter = one_of("+-")
        .or_not()
        .then(text::digits(10)) // accepting decimal digits only
        .to_slice()
        .padded()
        .validate(|s: &str, e, emitter| {
            let num = s.parse::<i32>();
            if num.is_err() {
                emitter.emit(Rich::custom(
                    e.span(),
                    "Integer Oversize: this is not a 32-bit integer.",
                ))
            }
            num.unwrap_or(0)
        })
        .map(Expr::IntLiter);

    let bool_liter = choice((
        text::ascii::keyword("true")
            .to_slice()
            .padded()
            .to(Expr::BoolLiter(true)),
        text::ascii::keyword("false")
            .to_slice()
            .padded()
            .to(Expr::BoolLiter(false)),
    ));

    // Regarding transformation:
    // Ideally we would recognize a specific pattern for escape characters and perform correct analysis
    // The character transformation only applies for <char-liter> and <str-liter>.
    // This should be an ascii character, and it should not parse unicodes.

    let graphic_ascii_char = any::<'a, &'a str, extra::Err<Rich<'a, char, Span>>>()
        .filter(char::is_ascii)
        .filter(|c| c >= &' ');

    let normal_char = graphic_ascii_char.and_is(none_of("\\\'\""));

    // <escaped-char> ::= ‘0’|‘b’|‘t’|‘n’|‘f’|‘r’|‘"’|‘'’|‘\’
    let escape_char = just('\\').ignore_then(choice((
        just('0').to('\0'),
        just('b').to('\x08'),
        just('t').to('\t'),
        just('n').to('\n'),
        just('f').to('\x0C'),
        just('r').to('\r'),
        just('\"').to('\"'),
        just('\'').to('\''),
        just('\\').to('\\'),
    )));

    let char_elem = normal_char
        .or(escape_char)
        .to_slice()
        .validate(move |s: &str, e, emitter| {
            let n = s.chars().count();
            if n == 1 {
                normal_char.parse(s).into_result().unwrap()
            } else if n == 2 {
                let try_escape = escape_char.parse(s);
                return if try_escape.has_errors() {
                    emitter.emit(Rich::custom(e.span(), "invalid escape character pattern"));
                    ' '
                } else {
                    try_escape.into_result().unwrap()
                };
            } else {
                emitter.emit(Rich::custom(e.span(), "invalid char: unaccepted pattern"));
                return ' ';
            }
        });

    // let char_str = normal_char.or(escape_char).to_slice()

    let char_liter = char_elem
        .delimited_by(just('\''), just('\''))
        .padded()
        .map(Expr::CharLiter);

    let str_preprocess = char_elem.clone().repeated();

    let str_parts = char_elem.repeated().collect::<String>();

    let str_liter = str_preprocess
        .to_slice()
        .delimited_by(just('\"'), just('\"'))
        .map(move |s| {
            println!("{}", s);
            let result = str_parts.parse(s).into_result().unwrap();
            Expr::StrLiter(result)
        });

    let token = int_liter.or(bool_liter).or(char_liter).or(str_liter);

    token.map_with(|tok, e| (tok, e.span()))
}

#[test]
fn can_parse_basic_int_liter() {
    let input = "3";
    let result = expr_atom_literal().parse(input).into_result();
    assert!(matches!(result, Ok((expr, _)) if expr == Expr::IntLiter(3)));
}

#[test]
fn cannot_parse_oversize_int_liter() {
    let input = "1000000000000000000";
    let result = expr_atom_literal().parse(input).into_result();
    assert!(result.is_err());
}

#[test]
fn can_parse_bool_liter() {
    let input = "true";
    let result = expr_atom_literal().parse(input).into_result();
    assert!(matches!(result, Ok((expr, _)) if expr == Expr::BoolLiter(true)));
}

#[test]
fn can_parse_char_literals() {
    let input = "\'e\'";
    let result = expr_atom_literal().parse(input).into_result();
    assert!(matches!(result, Ok((expr, _)) if expr == Expr::CharLiter('e')));
}

#[test]
fn can_parse_escape_chars() {
    let input = "\'\\n\'";
    let result = expr_atom_literal().parse(input).into_result();
    assert!(matches!(result, Ok((expr, _)) if expr == Expr::CharLiter('\n')));
}

#[test]
fn cannot_parse_invalid() {
    let input = "\'£\'";
    let result = expr_atom_literal().parse(input).into_result();
    assert!(result.is_err());
    // assert!(matches!(result, Ok((expr, _)) if expr == Expr::CharLiter('\n')));
}

#[test]
fn can_parse_string_literals() {
    let input = "\"hello\"";
    let result = expr_atom_literal().parse(input).into_result();
    assert!(matches!(result, Ok((expr, _)) if expr == Expr::StrLiter("hello".parse().unwrap())));
}

#[test]
fn can_parse_string_literals_with_escape() {
    let input = "\"a\\0b\"";
    let result = expr_atom_literal().parse(input).into_result();
    assert!(matches!(result, Ok((expr, _)) if expr == Expr::StrLiter("a\0b".parse().unwrap())));
}
