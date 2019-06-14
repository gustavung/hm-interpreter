use nom::{digit1, multispace0, multispace1};

#[derive(Debug)]
pub enum Expr {
    Var(String),
    App(Box<Expr>, Box<Expr>),
    Lam(String, Box<Expr>),
    Let(String, Box<Expr>, Box<Expr>),
    LetRec(String, Box<Expr>, Box<Expr>),
    BoolLit(bool),
    IntLit(i32),
    Comp(String),
    If(Box<Expr>, Box<Expr>, Box<Expr>)
}

named!(expr(&str) -> Expr, alt!(
        let_expr | letrec_expr | if_expr | lambda_expr | comp_expr | bool_expr | int_expr | var_expr | app_expr
    )
);

named!(paren_expr(&str) -> Expr, do_parse!(
        tag!("(")
        >> e: expression
        >> tag!(")")
        >> ( e )
    )
);

named!(pub expression(&str) -> Expr, alt!(call!(paren_expr) | call!(expr)));



named!(pub let_expr(&str) -> Expr, do_parse!(
        multispace0
        >> tag!("let")
        >> multispace1
        >> name: take_until_and_consume!(" ")
        >> multispace0
        >> tag!("=")
        >> multispace0
        >> expr1: expression
        >> multispace1
        >> tag!("in")
        >> multispace0
        >> expr2: expression
        >> ( Expr::Let(name.to_string(), Box::new(expr1), Box::new(expr2)) )
    )
);

named!(pub letrec_expr(&str) -> Expr, do_parse!(
        multispace0
        >> tag!("letrec")
        >> multispace1
        >> name: take_until_and_consume!(" ")
        >> multispace0
        >> tag!("=")
        >> multispace0
        >> expr1: expression
        >> multispace1
        >> tag!("in")
        >> multispace0
        >> expr2: expression
        >> ( Expr::Let(name.to_string(), Box::new(expr1), Box::new(expr2)) )
    )
);

named!(pub if_expr(&str) -> Expr, do_parse!(
        multispace0
        >> tag!("if")
        >> multispace1
        >> expr1: expression
        >> multispace1
        >> tag!("then")
        >> multispace1
        >> expr2: expression
        >> multispace1
        >> tag!("else")
        >> multispace1
        >> expr3: expression
        >> ( Expr::If(Box::new(expr1), Box::new(expr2), Box::new(expr3)) )
    )
);

named!(pub lambda_expr(&str) -> Expr, do_parse!(
        multispace0
        >> tag!(r#"\"#)
        >> name: take_until_and_consume!(".")
        >> expr: expression
        >> ( Expr::Lam(name.to_string(), Box::new(expr)) )
    )
);

named!(pub var_expr(&str) -> Expr, do_parse!(
        multispace0
        >> name: take_until_either_and_consume!(" \r\t\n")
        >> ( Expr::Var(name.to_string()) )
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

named!(pub comp_expr(&str) -> Expr, do_parse!(
        multispace0
        >> b: alt!(tag!(">") | tag!("<"))
        >> ( Expr::Comp(b.to_string()) )
    )
);
