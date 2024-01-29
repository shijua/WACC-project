use chumsky::prelude::SimpleSpan;

pub type Span = SimpleSpan<usize>;

pub type Spanned<T> = (T, Span);
