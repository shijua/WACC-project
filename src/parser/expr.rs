use crate::ast::{BinaryOperator, Expr, UnaryOperator};
use crate::parser::lexer::{ParserInput, Token};
use crate::{Span, Spanned};
use chumsky::error::Rich;
use chumsky::input::MapExtra;
use chumsky::pratt::{infix, left};
use chumsky::prelude::{choice, just, recursive, todo, Recursive};
use chumsky::IterParser;
use chumsky::{extra, select, Parser};

enum Associativity {
    Left,
    Right,
    NotAssoc,
}

const NORMALIZE_FACTOR: u32 = 10;

impl Associativity {
    // return (lbp, rbp, nbp)
    fn binding_powers(&self, binding: u32) -> (u32, u32, u32) {
        use Associativity::*;
        let n = binding * NORMALIZE_FACTOR;
        match self {
            Left => (n, n + 1, n),
            Right => (n, n, n),
            NotAssoc => (n, n + 1, n - 1),
        }
    }
}

impl BinaryOperator {
    fn associativity(&self) -> Associativity {
        use crate::ast::BinaryOperator::*;
        match self {
            Mul | Modulo | Div | Add | Sub => Associativity::Left,
            Lte | Lt | Gt | Gte | Eq | Neq => Associativity::NotAssoc,
            And | Or => Associativity::Right,
        }
    }

    fn precedence(&self) -> u32 {
        use crate::ast::BinaryOperator::*;
        match self {
            Or => 1,
            And => 2,
            Eq | Neq => 3,
            Gt | Gte | Lt | Lte => 4,
            Add | Sub => 5,
            Mul | Modulo | Div => 6,
        }
    }

    // Left Binding Power
    fn lbp(&self) -> u32 {
        self.associativity().binding_powers(self.precedence()).0
    }

    // Right Binding Power
    fn rbp(&self) -> u32 {
        self.associativity().binding_powers(self.precedence()).1
    }

    // Next Binding Power
    fn nbp(&self) -> u32 {
        self.associativity().binding_powers(self.precedence()).2
    }
}

fn expr<'tokens, 'src: 'tokens>() -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>,
    Spanned<Expr>,
    extra::Err<Rich<'tokens, Token<'src>, Span>>,
> + Clone {
    let mut expr = Recursive::declare();

    // let mut expr_binary = Recursive::declare();

    // for <int-liter>, <bool-liter>, <char-liter>, <str-liter>, <pair-liter>
    let atomic_liter = select! {
        Token::IntToken(x) => Expr::IntLiter(x),
        Token::BoolToken(x) => Expr::BoolLiter(x),
        Token::CharToken(x) => Expr::CharLiter(x),
        Token::StrToken(x) => Expr::StrLiter(x),
        Token::Keyword("null") => Expr::PairLiter,
    }
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
        .map(|(ident_name, indices_vec)| {
            Expr::ArrayElem(ident_name.to_string(), indices_vec.clone())
        });

    // <ident>
    let ident_expr = ident.map(|s| Expr::Ident(s.to_string()));

    // <atom> ::= <int-liter> | <bool-liter> | <char-liter> | <str-liter> | <pair-liter> |
    //            <ident>     | <array-elem> | <expr>
    let atom = choice((atomic_liter, array_elem, ident_expr))
        .map_with(|expr, e| (expr, e.span()))
        .or(expr // bracketed
            .clone()
            .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')'))))
        .boxed();

    // let expr_parser = atom.clone();

    // unary applications always have higher precedence than binary applications

    // <unary_oper> ::= '!' | '-' | 'len' | 'ord' | 'chr'
    let unary_oper = select! {
        Token::Op("!") => UnaryOperator::Bang,
        Token::Op("-") => UnaryOperator::Negative,
        Token::Keyword("len") => UnaryOperator::Len,
        Token::Keyword("chr") => UnaryOperator::Chr,
    }
    .labelled("unary operators");

    let expr_unary = unary_oper
        .then(expr.clone())
        .map(|(op, target)| Expr::UnaryApp(op, Box::new(target)))
        .map_with(|exp, e| (exp, e.span()));

    let expr_unary_atom = expr_unary.or(atom);

    // we use a Vec to mimic the function call parameters.
    // use parameter_stack.pop() to fetch the current.

    // <binary_oper> ::= * % /  + -   >= > <= <  == != && ||
    let binary_oper = select! {
      Token::Op("*") => BinaryOperator::Mul,
        Token::Op("%") => BinaryOperator::Modulo,
        Token::Op("/") => BinaryOperator::Div,
        Token::Op("+") => BinaryOperator::Add,
        Token::Op("-") => BinaryOperator::Sub,
        Token::Op(">=") => BinaryOperator::Gte,
        Token::Op(">") => BinaryOperator::Gt,
        Token::Op("<=") => BinaryOperator::Lte,
        Token::Op("<") => BinaryOperator::Lt,
        Token::Op("==") => BinaryOperator::Eq,
        Token::Op("!=") => BinaryOperator::Neq,
        Token::Op("&&") => BinaryOperator::And,
        Token::Op("||") => BinaryOperator::Or,
    };

    let mut powers: Vec<u32> = Vec::new();
    powers.push(0);

    let mut expr_binary = Recursive::declare();

    expr_binary.define({
        let mut bound: u32 = u32::MAX;
        let power = powers.pop().unwrap();
        expr_unary_atom.clone().foldl_with(
            binary_oper
                .filter(|binop| {
                    let current_lbp = binop.lbp();
                    let result = (power <= current_lbp) && (current_lbp <= bound);
                    if result {
                        powers.push(binop.rbp());
                    }
                    result
                })
                .then(expr_binary.clone())
                .repeated(),
            |a, (op, b), e| {
                let result = Expr::BinaryApp(Box::new(a), op, Box::new(b));
                bound = op.nbp();
                (result, e.span())
            },
        );
    });

    //
    choice((expr_unary, expr_binary, atom.clone()))
}
