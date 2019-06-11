use nom::{digit1, alpha};

#[derive(Debug)]
pub enum Expr {
    Var(String),
    App(Box<Expr>, Box<Expr>),
    Lam(String, Box<Expr>),
    Let(String, Box<Expr>, Box<Expr>),
    BoolLit(bool),
    IntLit(i32),
    If(Box<Expr>, Box<Expr>, Box<Expr>)
}

// TODO: handle integer and boolean literals
named!(pub expression(&str) -> Expr, alt!(
        let_expr | if_expr | lambda_expr | bool_expr | int_expr | var_expr | app_expr
    )
);

named!(pub let_expr(&str) -> Expr, do_parse!(
        eat_separator!(" \r\t\n") >>
        tag!("let") >>
        eat_separator!(" \r\t\n") >>
        name: take_until_and_consume!(" ") >>
        eat_separator!(" \r\t\n") >>
        tag!("=") >>
        eat_separator!(" \r\t\n") >>
        expr1: expression >>
        eat_separator!(" \r\t\n") >>
        tag!("in") >>
        eat_separator!(" \r\t\n") >>
        expr2: expression >>
        ( Expr::Let(name.to_string(), Box::new(expr1), Box::new(expr2)) )
    )
);

named!(pub if_expr(&str) -> Expr, do_parse!(
        eat_separator!(" \r\t\n") >>
        tag!("if") >>
        eat_separator!(" \r\t\n") >>
        expr1: expression >>
        eat_separator!(" \r\t\n") >>
        tag!("then") >>
        eat_separator!(" \r\t\n") >>
        expr2: expression >>
        eat_separator!(" \r\t\n") >>
        tag!("else") >>
        eat_separator!(" \r\t\n") >>
        expr3: expression >>
        ( Expr::If(Box::new(expr1), Box::new(expr2), Box::new(expr3)) )
    )
);

named!(pub lambda_expr(&str) -> Expr, do_parse!(
        eat_separator!(" \r\t\n") >>
        tag!(r#"\"#) >>
        name: take_until_and_consume!(".") >>
        expr: expression >>
        ( Expr::Lam(name.to_string(), Box::new(expr)) )
    )
);

named!(pub var_expr(&str) -> Expr, do_parse!(
        eat_separator!(" \r\t\n") >>
        name: take_until_either_and_consume!(" \r\t\n") >>
        ( Expr::Var(name.to_string()) )
    )
 );

named!(pub app_expr(&str) -> Expr, do_parse!(
        eat_separator!(" \r\t\n") >>
        expr1: expression >>
        eat_separator!(" \r\t\n") >>
        expr2: expression >>
        ( Expr::App(Box::new(expr1), Box::new(expr2)) )
    )
);

named!(pub bool_expr(&str) -> Expr, do_parse!(
        eat_separator!(" \r\t\n") >>
        b: alt!(tag!("true") => { |_| true } |
                tag!("false") => { |_| false } ) >>
        ( Expr::BoolLit(b) )
    )
);

named!(pub int_expr(&str) -> Expr, do_parse!(
        eat_separator!(" \r\t\n") >>
        i: map_res!(digit1, |integer: &str| integer.parse::<i32>())>>
        ( Expr::IntLit(i) )
    )
);
