// <Expr> (expressions) syntax parser & analysis

use crate::ast::BaseValue::*;
use crate::ast::{ArrayElem, Expr, Operator};
use crate::parser::lexer::{ParserInput, Span, Spanned, Token};
use chumsky::error::Rich;
use chumsky::pratt::{infix, left, prefix};
use chumsky::prelude::*;
use chumsky::recursive::recursive;
use chumsky::{extra, select, Parser};

fn unary_combine(op: Operator, r: Spanned<Expr>) -> Expr {
    Expr::UnaryApp(op, Box::new(r))
}

fn binary_combine<'src>(
    op: Operator,
    l: Spanned<Expr<'src>>,
    r: Spanned<Expr<'src>>,
) -> Expr<'src> {
    Expr::BinaryApp(op, Box::new(l), Box::new(r))
}

pub fn expr_parser<'tokens, 'src: 'tokens>() -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>,
    Spanned<Expr<'src>>,
    extra::Err<Rich<'tokens, Token<'src>, Span>>,
> + Clone {
    let base_expr = recursive(|expr| {
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
        let ident = select! {
            Token::Ident(x) => x,
        }
        .labelled("identifiers");

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
        let atom = base_value.or(array_elem).or(ident.map(Expr::Ident));

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
        // Now, 6 to 0 is from tightest bindings to weakest.
        // 6: prefix, ‘!’, ‘-’, ‘len’, ‘ord’, ‘chr’
        // 5: infix left, ‘*’, ‘%’, ‘/’
        // 4: infix left, ‘+’, ‘-’
        // 3: infix non, ‘>’, ‘>=’, ‘<’, ‘<=’
        // 2: infix non, ‘==’, ‘!=’
        // 1: infix right, ‘&&’
        // 0: infix right, ‘||’
        // Maybe Pratt Parsing?

        let atomic_parser = atom
            .clone()
            .map_with(|expr, e| (expr, e.span()))
            // bracketed expressions
            .or(expr
                .clone()
                .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')'))))
            // attempt to recover anything that looks like a parenthesised expression but contains errors
            .recover_with(via_parser(nested_delimiters(
                Token::Ctrl('('),
                Token::Ctrl(')'),
                [(Token::Ctrl('['), Token::Ctrl(']'))],
                |span| (Expr::Error, span),
            )))
            .boxed();

        let op = just(Token::Op("*")).to(Operator::Mul);
        let product = atomic_parser
            .clone()
            .foldl_with(op.then(atomic_parser).repeated(), |a, (op, b), e| {
                (Expr::BinaryApp(op, Box::new(a), Box::new(b)), e.span())
            });
        product
    });
    base_expr
    // let atomic = base_expr.clone().pratt((
    //     infix::<_, _, Operator, Expr>(
    //         left(4),
    //         just::<Token<'_>, _, extra::Err<Rich<'tokens, Token<'src>, Span>>>(
    //             Token::Op("+"),
    //         ),
    //         |l, r| binary_combine(Operator::Add, l, r),
    //     ),
    //     // infix(left(0), just(Token::Op("-")), |l, r| {
    //     //     binary_combine(Operator::Minus, l, r)
    //     // }),
    // ));
    // atomic //.map_with(|expr, e| (expr, e.span()))
}
