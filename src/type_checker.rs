use std::collections::{HashMap, BTreeSet};
use std::iter::FromIterator;

use uuid::Uuid;

use crate::parser::Expr;


/**
 * This code is based on http://dev.stephendiehl.com/fun/006_hindley_milner.html
 * as well as the following papers:
 * Generalizing Hindley-Milner Type Inference Algorithms by Heeren et al
 * Principal type-schemes for functional programs by Luis Damas and Robin Milner
 **/

type Var = String;

/// A type is defined as a type variable, a type const (eg. bool, int etc) or a arrow type.
#[derive(Debug, Clone)]
pub enum Type {
    // type variable
    TypeV(Var),
    // type const
    TCon(String),
    // Arrow type
    TArr(Box<Type>, Box<Type>)
}

/// A type scheme is defined as a list of possible type variables as well as a type.
#[derive(Debug, Clone)]
enum Scheme {
    Forall(Vec<Var>, Type)
}

#[derive(Debug, Clone)]
pub struct TypeEnv(HashMap<Var, Scheme>);

#[derive(Debug, Clone)]
pub struct Subst(HashMap<Var, Type>);

impl TypeEnv {
    pub fn new() -> TypeEnv {
        TypeEnv {
            0: HashMap::new()
        }
    }

    fn extend(mut self, (v, s): (Var, Scheme)) {
        self.0.insert(v, s).unwrap();
    }

    fn remove(mut self, var: Var) {
        self.0.remove(&var).unwrap();
    }
}

impl Subst {
    fn new() -> Subst {
        Subst {
            0: HashMap::new()
        }
    }

    fn compose(&mut self, mut m: Subst) {
        self.0.extend(m.0.drain());
    }
}

//let typeInt: Type = Type::TCon(String::from("Int"));
//let typeBool: Type = Type::TCon(String::from("Bool"));

/// This trait defines what is required to be able to substitute type variables in the implementor.
trait Substitutable<T> {
    /// Given a substitution map, apply the substitutions
    fn apply(&mut self, sub: &Subst) -> T;
    /// return the set of free type variables
    fn ftv(&mut self) -> BTreeSet<Var>;
}

impl Substitutable<Type> for Type {
    fn apply(&mut self, sub: &Subst) -> Type {
        match self {
            Type::TCon(s) => Type::TCon(s.to_string()),
            Type::TypeV(t) => match sub.0.get(&t.to_string()) {
                Some(t_p) => t_p.clone(),
                None => Type::TypeV(t.to_string())
            }
            Type::TArr(a1, a2) => Type::TArr(Box::new(a1.apply(&sub)), Box::new(a2.apply(&sub)))
        }
    }
    fn ftv(&mut self) -> BTreeSet<Var> {
        let mut set = BTreeSet::new();
        match self {
            Type::TCon(_) => set,
            Type::TypeV(t) => {
                set.insert(t.to_string());
                set
            },
            Type::TArr(a1, a2) => {
                set.union(&a1.ftv());
                set.union(&a2.ftv());
                set
            }
        }
    }
}

impl Substitutable<Scheme> for Scheme {
    fn apply(&mut self, sub: &Subst) -> Scheme {
        match self {
            Scheme::Forall(tv, t) => {
                Scheme::Forall(tv.clone(), t.apply(tv.iter()
                                              .fold(sub, |acc, x| {
                                                  // This is a really bad solution
                                                  acc.clone().0
                                                     .remove(x)
                                                     .unwrap(); acc
                                                 })))
            }
        }
    }

    fn ftv(&mut self) -> BTreeSet<Var> {
        match self {
            Scheme::Forall(tv, t) => {
                let mut set = BTreeSet::new();
                set.union(&t.ftv());
                set.difference(&BTreeSet::from_iter(tv.iter().cloned()));
                set
            }
        }
    }
}

impl Substitutable<TypeEnv> for TypeEnv {
    fn apply(&mut self, sub: &Subst) -> TypeEnv {
        // drains self, is this really what we want?
        TypeEnv(self.0.drain().map(|(s, mut val)| (s, val.apply(sub))).collect())
    }

    fn ftv(&mut self) -> BTreeSet<Var> {
        BTreeSet::new()
    }
}

/// A check intended to prevent cyclic type variables and therefore infinite types.
fn occurs_check<T: Substitutable<T>>(v: Var, t: &mut T) -> bool {
    t.ftv().contains(&v)
}

/// This function will generate a new, fresh type variable.
fn fresh_var() -> Type {
    Type::TypeV(Uuid::new_v4().to_hyphenated_string())
}

/// An implementation of the generalization rule described in the papers above.
fn generalize(te: &mut TypeEnv, mut t: Type) -> Scheme {
    let free_vars1 = t.ftv();
    let free_vars2 = &te.ftv();
    let mut fv = free_vars1.difference(free_vars2);
    let mut vec = Vec::new();
    vec.extend(fv.cloned());
    Scheme::Forall(vec, t)
}

/// An implementation of the instantiation rule described in the papers above.
fn instantiate(s: &mut Scheme) -> Type {
    if let Scheme::Forall(v, t) = s {
        let mut subst = Subst::new();
        v.iter().map(|st| subst.0.insert(st.to_string(), fresh_var()));
        return t.apply(&subst)
    }
    // else error
    Type::TCon("a".to_string())
}

// TODO: Implement occurschecking below
/// This function will try to compute a substituation such that ty1 == ty2.apply(subs) is true.
fn unify(ty1: Type, ty2: Type) -> Subst {
    let mut sub = Subst::new();
    if let Type::TypeV(s) = ty1 {
        sub.0.insert(s, ty2);
    } else if let Type::TypeV(s) = ty2 {
        sub.0.insert(s, ty1);
    } else if let Type::TArr(a1, mut a2) = ty1 {
        if let Type::TArr(b1, mut b2) = ty2 {
            sub = unify(*a1, *b1);
            sub.compose(unify(a2.apply(&sub), b2.apply(&sub)));
        }
    } else {
        // something went wrong
    }
    sub
}

/// The main type inference algorithm. It is described as "algorithm W" in the litterature.
pub fn infer(ty_env: &mut TypeEnv, e: Expr) -> (Subst, Type) {
    match e {
        Expr::Ident(i) => {
            match ty_env.0.get_mut(&i) {
                Some(sc) => {
                    let inst = instantiate(sc);
                    (Subst::new(), inst)
                }
                None => {
                    println!("unbound var");
                    (Subst::new(), Type::TypeV("err".to_string()))
                }
            }
        }
        Expr::Lam(s, e) => {
            let fresh = fresh_var();
            ty_env.0.insert(s, Scheme::Forall(Vec::new(), fresh.clone()));
            let (s1, e1) = infer(ty_env, *e);
            (s1.clone(), Type::TArr(Box::new(fresh), Box::new(e1)).apply(&s1))
        }
        Expr::App(e1, e2) => {
            let mut tv = fresh_var();
            let (mut s1, mut t1) = infer(ty_env, *e1);
            let (mut s2, mut t2) = infer(&mut ty_env.apply(&s1), *e2);
            let mut s3 = unify(t1.apply(&s2), Type::TArr(Box::new(t2), Box::new(tv.clone())));
            let mut tv_p = tv.apply(&s3);
            s2.compose(s3);
            s1.compose(s2);
            (s1, tv_p)
        }
        Expr::Let(s, e1, e2) => {
            let (mut s1, mut t1) = infer(ty_env, *e1);
            let mut env_p = ty_env.apply(&s1);
            env_p.0.insert(s, generalize(&mut (env_p.clone()), t1));
            let (mut s2, mut t2) = infer(&mut env_p, *e2);
            s1.compose(s2);
            (s1, t2)
        }
        Expr::IntLit(i) => {
            (Subst::new(), Type::TCon("int".to_string()))
        }
        Expr::BoolLit(b) => {
            (Subst::new(), Type::TCon("bool".to_string()))
        }
        _ => {
            (Subst::new(), Type::TypeV("err".to_string()))
        }
    }
}
