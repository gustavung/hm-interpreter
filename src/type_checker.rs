use std::collections::{HashMap, BTreeSet};

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
enum Scheme {
    Forall(Vec<Var>, Type)
}

//let typeInt: Type = Type::TCon(String::from("Int"));
//let typeBool: Type = Type::TCon(String::from("Bool"));

/// This trait defines what is required to be able to substitute type variables in the implementor.
trait Substitutable<T> {
    /// Given a substitution map, apply the substitutions
    fn apply(mut self, sub: &Subst) -> T;
    /// return the set of free type variables
    fn ftv(mut self) -> BTreeSet<Var>;
}

impl Substitutable<Type> for Type {
    fn apply(mut self, sub: &Subst) -> Type {
        match self {
            Type::TCon(s) => Type::TCon(s),
            Type::TypeV(t) => sub.0.get(&t).unwrap().clone(),
            Type::TArr(a1, a2) => Type::TArr(Box::new(a1.apply(&sub)), Box::new(a2.apply(&sub)))
        }
    }
    fn ftv(mut self) -> BTreeSet<Var> {
        let mut set = BTreeSet::new();
        match self {
            Type::TCon(_) => set,
            Type::TypeV(t) => {
                set.insert(t);
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
    fn apply(mut self, sub: &Subst) -> Scheme {
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

    fn ftv(mut self) -> BTreeSet<Var> {
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

struct TypeEnv(HashMap<Var, Scheme>);

#[derive(Debug, Clone)]
struct Subst(HashMap<Var, Type>);

impl TypeEnv {
    fn new(mut self) {
        self.0 = HashMap::new();
    }

    fn extend(mut self, (v, s): (Var, Scheme)) {
        self.0.insert(v, s).unwrap();
    }
}

impl Subst {
    fn new(mut self) {
        self.0 = HashMap::new();
    }

    fn compose(mut self, mut m: Subst) {
        self.0.extend(m.0.drain())
    }
}
