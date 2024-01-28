// <Expr> (expressions) syntax parser & analysis

use crate::ast::BaseValue::*;
use crate::ast::{ArrayElem, Expr, Operator};
use crate::parser::lexer::{ParserInput, Span, Spanned, Token};
use chumsky::error::Rich;
use chumsky::pratt::{infix, prefix};
use chumsky::prelude::*;
use chumsky::recursive::recursive;
use chumsky::{extra, select, Parser};

pub fn expr_parser<'tokens, 'src: 'tokens>() -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>,
    Spanned<Expr<'src>>,
    extra::Err<Rich<'tokens, Token<'src>, Span>>,
> + Clone {
    recursive(|expr| {
        // The Basic Literal Values of Expr
        let base_value = select! {
            Token::Null => Expr::BaseValue(Null),
            Token::IntToken(x) => Expr::BaseValue(IntVal(x)),
            Token::Bool(x) => Expr::BaseValue(BoolVal(x)),
            Token::CharToken(x) => Expr::BaseValue(CharVal(x)),
            Token::StrToken(x) => Expr::BaseValue(StrVal(x)),
        }
        .labelled("base value");

        // Identifiers
        // TODO: What about Scoping?
        let ident = select! {
            Token::Ident(x) => x,
        }
        .labelled("identifiers");

        // Bracketed Expressions
        // let bracketed = expr
        //     .clone()
        //     .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')')))
        //     .map(|inner_expr| Expr::Bracketed(Box::new(inner_expr)));

        // TODO: Array Parsing
        // Array indices
        let array_indices = expr
            .clone()
            .delimited_by(just(Token::Ctrl('[')), just(Token::Ctrl(']')))
            .repeated()
            .at_least(1)
            .collect::<Vec<_>>();
        let array_elem = ident
            .clone()
            .then(array_indices)
            .map(|(ident_name, indices_vec)| {
                Expr::ArrayElem(ArrayElem {
                    ident: ident_name,
                    indices: indices_vec.clone(),
                })
            });
        // map array_elem to the ArrayElem Structure

        // 'Atom' are expressions (at this stage) without possibility of ambiguity
        let atom = base_value
            .or(array_elem)
            .or(ident.map(Expr::Ident))
            .map_with(|expr, e| (expr, e.span()))
            .or(expr
                .clone()
                .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')'))))
            // Attempt to recover anything that looks like a parenthesised expression but contains errors
            // .recover_with(via_parser(nested_delimiters(
            //     Token::Ctrl('('),
            //     Token::Ctrl(')'),
            //     [(Token::Ctrl('['), Token::Ctrl(']'))],
            //     |span| (Expr::Error, span),
            // )))
            ;

        // Then we come to handling unary applications and binary applications.
        // Chumsky library provides us with the newest "pratt parsing" functionality.
        // which provides easy classification including associativity functionalities.
        // We would first parse out the operators.
        // NOTE: Token::Op("-") could both be recognized as Unary or Binary,
        // However Pratt would handle this (hopefully) and we only need to combine them as one.
        // let operator = select! {
        //     Token::Op("!") => Operator::Bang,
        //     Token::Op("len") => Operator::Len,
        //     Token::Op("chr") => Operator::Chr,
        //     Token::Op("ord") => Operator::Ord,
        //     Token::Op("-") => Operator::Minus,
        //     Token::Op("*") => Operator::Mul,
        //     Token::Op("/") => Operator::Div,
        //     Token::Op("%") => Operator::Modulo,
        //     Token::Op("+") => Operator::Add,
        //     Token::Op(">") => Operator::Gt,
        //     Token::Op(">=") => Operator::Gte,
        //     Token::Op("<") => Operator::Lt,
        //     Token::Op("<=") => Operator::Lte,
        //     Token::Op("==") => Operator::Eq,
        //     Token::Op("!=") => Operator::Neq,
        //     Token::Op("&&") => Operator::And,
        //     Token::Op("||") => Operator::Or,
        // };

        // We would be needing a precedence table for pratt parsing.
        // The WACC Specification has already given us one, but the precedence in that
        // should be flipped to match Parser::pratt.
        // Now, 7 to 1 is from tightest bindings to weakest.
        // 7: prefix, ‘!’, ‘-’, ‘len’, ‘ord’, ‘chr’
        // 6: infix left, ‘*’, ‘%’, ‘/’
        // 5: infix left, ‘+’, ‘-’
        // 4: infix non, ‘>’, ‘>=’, ‘<’, ‘<=’
        // 3: infix non, ‘==’, ‘!=’
        // 2: infix right, ‘&&’
        // 1: infix right, ‘||’
        // TODO: Implement Pratt Parsing (see official doc for examples)

        atom.clone()
    })
}
