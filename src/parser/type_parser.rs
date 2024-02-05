use crate::ast::Type;
use crate::parser::lexer::{lexer, ParserInput, Token};
use crate::{Span, Spanned};
use chumsky::error::Rich;
use chumsky::input::Input;
use chumsky::prelude::{just, none_of, todo, Recursive};
use chumsky::recursive::recursive;
use chumsky::{extra, select, Parser};

fn type_parse<'tokens, 'src: 'tokens>() -> impl Parser<
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

    let ambiguous_pair = just(Token::Keyword("pair"))
        .then(none_of(Token::Ctrl('(')).rewind())
        .map_with(|_, e| {
            Type::Pair(
                Box::new((Type::Any, e.span())),
                Box::new((Type::Any, e.span())),
            )
        })
        // .map_with(|t, e| (t, e.span()))
        .labelled("general pairs");

    recursive(|type_parse| {
        let array_type = type_parse
            .clone()
            .then(just(Token::Ctrl('[')).then(just(Token::Ctrl(']'))))
            .map(|(x, _)| Type::Array(Box::new(x)))
            //.map_with(|t, e| (t, e.span()))
            .labelled("array type");

        let baseline_parser = array_type.clone().or(basic_cases);

        let pair_elem_type = baseline_parser
            .clone()
            .or(ambiguous_pair)
            .map_with(|t, e| (t, e.span()));

        let pair_type = just(Token::Keyword("pair"))
            .ignore_then(just(Token::Ctrl('(')))
            .ignore_then(pair_elem_type.clone())
            .then_ignore(just(Token::Ctrl(',')))
            .then(pair_elem_type.clone())
            .then_ignore(just(Token::Ctrl(')')))
            .map_with(|(l_elem, r_elem), e| {
                (Type::Pair(Box::new(l_elem), Box::new(r_elem)), e.span())
            });

        basic_cases
            .or(array_type)
            .clone()
            .map_with(|t, e| (t, e.span()))
            .or(pair_type)
    })
}

#[test]
fn can_parse_type() {
    let src = "int";
    let tokens = lexer().parse(src).into_result().unwrap();
    let expression = type_parse()
        .parse(tokens.as_slice().spanned((src.len()..src.len()).into()))
        .into_result();
    assert!(expression.is_ok());
}
