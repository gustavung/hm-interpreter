use std::collections::{HashMap, BTreeSet};

use uuid::Uuid;

use std::iter::FromIterator;

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

struct TypeEnv(HashMap<Var, Scheme>);

#[derive(Debug, Clone)]
struct Subst(HashMap<Var, Type>);

impl TypeEnv {
    fn new() -> TypeEnv {
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
        self.0.extend(m.0.drain())
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
            Type::TypeV(t) => sub.0.get(&t.to_string()).unwrap().clone(),
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

fn occurs_check<T: Substitutable<T>>(v: Var, t: &mut T) -> bool {
    t.ftv().contains(&v)
}

fn fresh_var() -> Var {
    Uuid::new_v4().to_hyphenated_string()
}

fn generalize(te: &mut TypeEnv, mut t: Type) -> Scheme {
    let free_vars1 = t.ftv();
    let free_vars2 = &te.ftv();
    let mut fv = free_vars1.difference(free_vars2);
    let mut vec = Vec::new();
    vec.extend(fv.cloned());
    Scheme::Forall(vec, t)
}

fn instantiate(s: Scheme) -> Type {
    if let Scheme::Forall(mut v, mut t) = s {
        let mut subst = Subst::new();
        v.iter().map(|st| subst.0.insert(st.to_string(), Type::TypeV(fresh_var())));
        return t.apply(&subst)
    }
    // else error
    Type::TCon("a".to_string())
}

// TODO: Implement occurschecking below
fn unify(ty1: Type, ty2: Type) -> Subst {
    let mut sub = Subst::new();
    if let Type::TypeV(s) = ty1 {
        sub.0.insert(s, ty2).unwrap();
    } else if let Type::TypeV(s) = ty2 {
        sub.0.insert(s, ty1).unwrap();
    } else if let Type::TArr(a1, mut a2) = ty1 {
        if let Type::TArr(b1, mut b2) = ty2 {
            sub = unify(*a1, *b1);
            let mut s2 = unify(a2.apply(&sub), b2.apply(&sub));
            sub.compose(s2);
        }
    } else {
        // something went wrong
    }
    sub
}
