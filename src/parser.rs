#[derive(Debug)]
pub enum Expr {
    Var(String),
    App(Box<Expr>, Box<Expr>),
    Lam(String, Box<Expr>),
    Let(String, Box<Expr>, Box<Expr>),
    Lit(u32),
    If(Box<Expr>, Box<Expr>, Box<Expr>)
}

named!(pub lambda(&str) -> Expr, do_parse!(
        tag!(r#"\"#) >>
        name: take_until_and_consume!(".") >>
        expr: expression >>
        ( Expr::Lam(name.to_string(), Box::new(expr)) )
    )
);

named!(pub var(&str) -> Expr, do_parse!(
        eat_separator!(" \r\t\n") >>
        name: take_until_either_and_consume!(" \r\t\n") >>
        ( Expr::Var(name.to_string()) )
    )
 );

named!(pub expression(&str) -> Expr, alt!(
         var | lambda
    )
);
