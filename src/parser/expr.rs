use crate::ast::{ArrayElem, BinaryOperator, Expr, UnaryOperator};
use crate::parser::lexer::{lexer, ParserInput, Token};
use crate::{Span, Spanned};
use chumsky::error::Rich;
use chumsky::input::{Input, MapExtra};
use chumsky::pratt::{infix, non, prefix};
use chumsky::prelude::{choice, just, recursive};
use chumsky::IterParser;
use chumsky::{extra, select, Parser};

pub fn expr<'tokens, 'src: 'tokens>() -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>,
    Spanned<Expr>,
    extra::Err<Rich<'tokens, Token<'src>, Span>>,
> + Clone {
    recursive(|expr| {
        // TODO: Edge Case: -2^31;
        let int_base = select! {Token::IntToken(x) => x}.labelled("int, unsigned");

        let plus_int = just(Token::Op("+"))
            .then(int_base.clone())
            .try_map(|(_, x), s| {
                let result = i32::try_from(x);
                return if result.is_err() {
                    Err(Rich::custom(s, "Integer out of range"))
                } else {
                    Ok(Expr::IntLiter(result.unwrap()))
                };
            });

        let minus_int = just(Token::Op("-"))
            .then(int_base.clone())
            .try_map(|(_, x), s| {
                let result = i32::try_from(-x);
                return if result.is_err() {
                    Err(Rich::custom(s, "Integer out of range"))
                } else {
                    Ok(Expr::IntLiter(result.unwrap()))
                };
            });

        let unsigned_int = int_base.clone().try_map(|x, s| {
            let result = i32::try_from(x);
            return if result.is_err() {
                Err(Rich::custom(s, "Integer out of range"))
            } else {
                Ok(Expr::IntLiter(result.unwrap()))
            };
        });

        let int_expr_parser = plus_int.or(minus_int).or(unsigned_int);

        // for <int-liter>, <bool-liter>, <char-liter>, <str-liter>, <pair-liter>
        let atomic_liter = select! {
            Token::BoolToken(x) => Expr::BoolLiter(x),
            Token::CharToken(x) => Expr::CharLiter(x),
            Token::StrToken(x) => Expr::StrLiter(x),
            Token::Keyword("null") => Expr::PairLiter,
        }
        .or(int_expr_parser)
        .labelled("base value");

        let ident = select! {
            Token::Ident(id) => id
        }
        .labelled("identifier");

        // <array-elem> ::= <ident> ('[' <expr> ']') +
        let array_elem = ident
            .clone()
            .then(
                expr.clone()
                    .delimited_by(just(Token::Ctrl('[')), just(Token::Ctrl(']')))
                    .repeated()
                    .at_least(1)
                    .collect::<Vec<_>>(),
            )
            .map_with(|(ident_name, indices_vec), e| {
                Expr::ArrayElem((
                    ArrayElem {
                        ident: String::from(ident_name),
                        indices: indices_vec,
                    },
                    e.span(),
                ))
            });

        // <ident>
        let ident_expr = ident.map(|s| Expr::Ident(s.to_string()));

        // <atom> ::= <int-liter> | <bool-liter> | <char-liter> | <str-liter> | <pair-liter> |
        //            <ident>     | <array-elem> | <expr>
        let atom = choice((array_elem, atomic_liter, ident_expr))
            .map_with(|expr, e| (expr, e.span()))
            .or(expr
                .clone()
                .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')'))))
            .boxed();

        // <unary_oper> ::= '!' | '-' | 'len' | 'ord' | 'chr'
        let unary_oper = select! {
            Token::Op("!") => UnaryOperator::Bang,
            Token::Op("-") => UnaryOperator::Negative,
            Token::Keyword("len") => UnaryOperator::Len,
            Token::Keyword("chr") => UnaryOperator::Chr,
            Token::Keyword("ord") => UnaryOperator::Ord,
        }
        .labelled("unary operator");

        let binary_mul = select! {
            Token::Op("*") => BinaryOperator::Mul,
            Token::Op("%") => BinaryOperator::Modulo,
            Token::Op("/") => BinaryOperator::Div,
        }
        .labelled("binary operator");

        let binary_add = select! {
            Token::Op("+") => BinaryOperator::Add,
            Token::Op("-") => BinaryOperator::Sub,
        }
        .labelled("binary operator");

        let binary_gte = select! {
            Token::Op(">=") => BinaryOperator::Gte,
            Token::Op(">") => BinaryOperator::Gt,
            Token::Op("<=") => BinaryOperator::Lte,
            Token::Op("<") => BinaryOperator::Lt,
        }
        .labelled("binary operator");

        let binary_eq = select! {
            Token::Op("!=") => BinaryOperator::Neq,
            Token::Op("==") => BinaryOperator::Eq,
        }
        .labelled("binary operator");

        let binary_and = just(Token::Op("&&"))
            .to(BinaryOperator::And)
            .labelled("binary operator");

        let binary_or = just(Token::Op("||"))
            .to(BinaryOperator::Or)
            .labelled("binary operator");

        let binary_fold = |lhs, op, rhs, e: &mut MapExtra<'tokens, '_, _, _>| {
            (Expr::BinaryApp(Box::new(lhs), op, Box::new(rhs)), e.span())
        };

        let unary_fold = |op, rhs, e: &mut MapExtra<'tokens, '_, _, _>| {
            (Expr::UnaryApp(op, Box::new(rhs)), e.span())
        };

        atom.pratt((
            prefix(7, unary_oper, unary_fold),
            infix(non(6), binary_mul, binary_fold),
            infix(non(5), binary_add, binary_fold),
            infix(non(4), binary_gte, binary_fold),
            infix(non(3), binary_eq, binary_fold),
            infix(non(2), binary_and, binary_fold),
            infix(non(1), binary_or, binary_fold),
        ))
        .boxed()
    })
}

#[test]
fn can_parse_expr() {
    let src = "1 < 2";
    let tokens = lexer().parse(src).into_result().unwrap();
    let expression = expr()
        .parse(tokens.as_slice().spanned((src.len()..src.len()).into()))
        .into_result();
    assert!(expression.is_ok());
}

#[test]
fn can_parse_plus_plus_expr() {
    let src = "1++2";
    let tokens = lexer().parse(src).into_result().unwrap();
    let expression = expr()
        .parse(tokens.as_slice().spanned((src.len()..src.len()).into()))
        .into_result();
    assert!(expression.is_ok());
}
