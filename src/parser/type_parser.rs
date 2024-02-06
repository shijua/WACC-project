use crate::ast::Type;
use crate::parser::lexer::{lexer, ParserInput, Token};
use crate::{Span, Spanned};
use chumsky::error::Rich;
use chumsky::prelude::{Input, just};
use chumsky::recursive::recursive;
use chumsky::{extra, select, Parser};

pub fn type_parse<'tokens, 'src: 'tokens>() -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>,
    Spanned<Type>,
    extra::Err<Rich<'tokens, Token<'src>, Span>>,
> + Clone {
    let basic_cases = select! {
        Token::Keyword("int") => Type::IntType,
        Token::Keyword("bool") => Type::BoolType,
        Token::Keyword("char") => Type::CharType,
        Token::Keyword("string") => Type::StringType,
    };

    let base_type = basic_cases
        .clone()
        .map_with(|t, e| (t, e.span()))
        .labelled("base type");

    recursive(|type_parse| {
        let pair_type = {
            // let pair_elem_base = type_parse.clone().validate();

            let pair_elem_type = type_parse
                .clone()
                .filter(|(x, _x_span)| !matches!(x, Type::Pair(_, _)))
                .or(just(Token::Keyword("pair")).map_with(|_, e| (Type::NestedPair, e.span()))); //.and_is(base_type.not());

            just(Token::Keyword("pair"))
                .ignore_then(just(Token::Ctrl('(')))
                .ignore_then(pair_elem_type.clone())
                .then(just(Token::Ctrl(',')))
                .then(pair_elem_type.clone())
                .then(just(Token::Ctrl(')')))
                .map_with(|(((p1, _), p2), _), e| {
                    (Type::Pair(Box::new(p1), Box::new(p2)), e.span())
                })
        };

        let array_base = base_type.or(pair_type);

        array_base.clone().foldl_with(
            just(Token::Ctrl('['))
                .then(just(Token::Ctrl(']')))
                .repeated(),
            |base, _, e| (Type::Array(Box::new(base)), e.span()),
        )
    })
}

#[test]
fn can_parse_basic_type() {
    let src = "int";
    let tokens = lexer().parse(src).into_result().unwrap();
    let expression = type_parse()
        .parse(tokens.as_slice().spanned((src.len()..src.len()).into()))
        .into_result();
    assert!(expression.is_ok());
}

#[test]
fn can_parse_array_type() {
    let src = "char[]";
    let tokens = lexer().parse(src).into_result().unwrap();
    let expression = type_parse()
        .parse(tokens.as_slice().spanned((src.len()..src.len()).into()))
        .into_result();
    assert!(expression.is_ok());
}

#[test]
fn can_parse_pair_type() {
    let src = "pair(pair(int, int)[], int)";
    let tokens = lexer().parse(src).into_result().unwrap();
    let expression = type_parse()
        .parse(tokens.as_slice().spanned((src.len()..src.len()).into()))
        .into_result();
    assert!(expression.is_ok());
}

#[test]
fn can_parse_pair_with_literals() {
    let src = "pair(int, pair)";
    let tokens = lexer().parse(src).into_result().unwrap();
    let expression = type_parse()
        .parse(tokens.as_slice().spanned((src.len()..src.len()).into()))
        .into_result();
    assert!(expression.is_ok());
}

#[test]
fn cannot_parse_ambiguous_pair() {
    let src = "pair(pair[], int)";
    let tokens = lexer().parse(src).into_result().unwrap();
    let expression = type_parse()
        .parse(tokens.as_slice().spanned((src.len()..src.len()).into()))
        .into_result();
    assert!(expression.is_err());
}

#[test]
fn cannot_parse_nested_pair() {
    let src = "pair(pair(int, int), int)";
    let tokens = lexer().parse(src).into_result().unwrap();
    let expression = type_parse()
        .parse(tokens.as_slice().spanned((src.len()..src.len()).into()))
        .into_result();
    assert!(expression.is_err());
}
