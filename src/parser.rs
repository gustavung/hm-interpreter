use nom::character::complete::{digit1, multispace0, multispace1, alphanumeric1};
use nom::IResult;
use nom::branch::alt;
use nom::combinator::{complete, map, map_res, not, peek};
use nom::sequence::{preceded, terminated};
use nom::character::complete::char;
use nom::bytes::complete::tag;

#[derive(Debug)]
pub enum Expr {
    Ident(String),
    App(Box<Expr>, Box<Expr>),
    Lam(String, Box<Expr>),
    Let(String, Box<Expr>, Box<Expr>),
    LetRec(String, Box<Expr>, Box<Expr>),
    BoolLit(bool),
    IntLit(i32),
    MinLit(Box<Expr>),
    Add(Box<Expr>, Box<Expr>),
    Comp(Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>)
}

/*
named!(comment(&str) -> &str, preceded!(
        tag!("//"),
        take_until!("\n")
    )
);*/

/*

Expression ::= Paren_expr | Expr
Paren_expr ::= ( + Expression + )
Expr ::= Let | LetRec | If | Lambda | App | Atom
Atom ::= Bool | Int | Ident | Paren_expr

Let ::= let Ident = + Expression + in + Expression
LetRec ::= letrec Ident = + Expression + in + Expression
If ::= if + Expression + then + Expression + else + Expression
Lambda ::= \ + Ident + . + Expression
App ::= Atom Atom

*/


/*
TODO:   1) Fix parsing for false positives, e.g. let x=1 inx
        2) Fix parsing for variable names with keywords in them, e.g. letx
*/

fn ws<'a>(s: &'a str) -> impl Fn(&'a str) -> IResult<&'a str, &'a str> {
    move |input: &str| {
        terminated(preceded(multispace0, tag(s)), multispace0)(input)
    }
}

fn keywords<'a>(i: &'a str) -> IResult<&'a str, &'a str> {
    alt((
        tag("let"),
        tag("letrec"),
        tag("in"),
        tag("="),
        tag("if"),
        tag("then"),
        tag("else")
    ))(i)
}

pub fn parse(i: &str) -> IResult<&str, Expr> {
    preceded(multispace0, parse_expression)(i)
}

fn parse_expression(i: &str) -> IResult<&str, Expr> {
    alt((parse_expr, parse_paren_expr))(i)
}

fn parse_expr(i: &str) -> IResult<&str, Expr> {
    alt((complete(parse_let_expr),
        complete(parse_letrec_expr),
        complete(parse_if_expr),
        complete(parse_lambda_expr),
        complete(parse_comp_expr),
        complete(parse_add_expr),
        complete(parse_app_expr),
        complete(parse_atom),
        ))(i)
}

fn parse_paren_expr(i: &str) -> IResult<&str, Expr> {
    preceded(char('('), terminated(parse_expression, char(')')))(i)
}

fn parse_atom(i: &str) -> IResult<&str, Expr> {
    alt((complete(parse_bool_expr),
        complete(parse_int_expr),
        complete(parse_ident_expr),
        complete(parse_min_expr),
        complete(parse_paren_expr),
        ))(i)
}

fn parse_let_expr(i: &str) -> IResult<&str, Expr> {
    let (i, name) = preceded(terminated(tag("let"), multispace1), map(alphanumeric1, |t: &str| t.to_string()))(i)?;
    let (i, expr1) = preceded(ws("="), parse_expression)(i)?;
    let (i, expr2) = preceded(ws("in"), parse_expression)(i)?;
    Ok((i, Expr::Let(name.to_string(), Box::new(expr1), Box::new(expr2))))
}

fn parse_letrec_expr(i: &str) -> IResult<&str, Expr> {
    let (i, name) = preceded(terminated(tag("letrec"), multispace1), map(alphanumeric1, |t: &str| t.to_string()))(i)?;
    let (i, expr1) = preceded(ws("="), parse_expression)(i)?;
    let (i, expr2) = preceded(ws("in"), parse_expression)(i)?;
    Ok((i, Expr::LetRec(name.to_string(), Box::new(expr1), Box::new(expr2))))
}

fn parse_if_expr(i: &str) -> IResult<&str, Expr> {
    let (i, expr1) = preceded(ws("if"), parse_expression)(i)?;
    let (i, expr2) = preceded(ws("then"), parse_expression)(i)?;
    let (i, expr3) = preceded(ws("else"), parse_expression)(i)?;
    Ok((i, Expr::If(Box::new(expr1), Box::new(expr2), Box::new(expr3))))
}

fn parse_lambda_expr(i: &str) -> IResult<&str, Expr> {
    let (i, name) = preceded(tag(r#"\"#), map(alphanumeric1, |t: &str| t.to_string()))(i)?;
    let (i, expr) = preceded(terminated(tag("."), multispace1), parse_expression)(i)?;
    Ok((i, (Expr::Lam(name.to_string(), Box::new(expr)))))
}

fn parse_ident_expr(i: &str) -> IResult<&str, Expr> {
    let (i, _) = peek(not(keywords))(i)?;
    let (i, name) = map(alphanumeric1, |t: &str| t.to_string())(i)?;
    Ok((i, ( Expr::Ident(name.to_string()) )))
}

fn parse_app_expr(i: &str) -> IResult<&str, Expr> {
    let (i, expr1) = parse_atom(i)?;
    let (i, expr2) = preceded(multispace1, parse_atom)(i)?;
    Ok((i, ( Expr::App(Box::new(expr1), Box::new(expr2)) )))
}

fn parse_bool_expr(i: &str) -> IResult<&str, Expr> {
    let (i, b) = alt((map(tag("true"), |_| true), map(tag("false"), |_| false)))(i)?;
    Ok((i, Expr::BoolLit(b)))
}

fn parse_int_expr(i: &str) -> IResult<&str, Expr> {
    let (i, b) = map_res(digit1, |integer: &str| integer.parse::<i32>())(i)?;
    Ok((i,  Expr::IntLit(b)))
}

fn parse_comp_expr(i: &str) -> IResult<&str, Expr> {
    let (i, expr1) = parse_atom(i)?;
    let (i, expr2) = preceded(ws("<"), parse_atom)(i)?;
    Ok((i,  Expr::Comp(Box::new(expr1), Box::new(expr2))))
}

fn parse_add_expr(i: &str) -> IResult<&str, Expr> {
    let (i, expr1) = parse_atom(i)?;
    let (i, expr2) = preceded(ws("+"), parse_atom)(i)?;
    Ok((i,  Expr::Add(Box::new(expr1), Box::new(expr2))))
}

fn parse_min_expr(i: &str) -> IResult<&str, Expr> {
    let (i, expr1) = preceded(ws("-"), parse_atom)(i)?;
    Ok((i,  Expr::MinLit(Box::new(expr1))))
}
