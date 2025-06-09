use im::HashMap;

use crate::typecheck::{Type, TypeError, TypeVar};

#[derive(Clone, Debug, Default)]
pub struct Subst(HashMap<TypeVar, Type>);

impl Subst {
    pub fn empty() -> Self {
        Self::default()
    }

    pub fn unit(var: TypeVar, ty: Type) -> Self {
        Self(HashMap::unit(var, ty))
    }

    pub fn get(&self, var: TypeVar) -> Option<&Type> {
        self.0.get(&var)
    }

    pub fn extend(&self, var: TypeVar, ty: Type) -> Self {
        let mut new_subst = self.clone();
        new_subst.0.insert(var, ty);
        new_subst
    }

    pub fn compose(self, other: Self) -> Self {
        self.0
            .into_iter()
            .fold(other, |acc, (var, ty)| acc.extend(var, acc.apply(ty)))
    }

    pub fn apply(&self, ty: Type) -> Type {
        match ty {
            Type::Unit | Type::Num | Type::Bool => ty,
            Type::Var(var) => self.get(var).cloned().unwrap_or(ty),
            Type::Fn(arg, ret) => Type::Fn(Box::new(self.apply(*arg)), Box::new(self.apply(*ret))),
            Type::Tuple(items) => {
                Type::Tuple(items.into_iter().map(|item| self.apply(item)).collect())
            }
        }
    }
}

// Check if a type variable occurs in a type (occurs check)
fn occurs_in(var: TypeVar, ty: &Type) -> bool {
    match ty {
        Type::Unit | Type::Num | Type::Bool => false,
        Type::Var(v) => *v == var,
        Type::Fn(lhs, rhs) => occurs_in(var, lhs) || occurs_in(var, rhs),
        Type::Tuple(items) => items.iter().any(|item| occurs_in(var, item)),
    }
}

/// Unifies two types and returns a substitution that makes them equal.
/// Returns an error if the types cannot be unified.
pub fn unify(ty1: Type, ty2: Type) -> Result<Subst, TypeError> {
    match (ty1, ty2) {
        // Same types unify trivially
        (t1, t2) if t1 == t2 => Ok(Subst::empty()),

        // Type variables unify with any type
        (Type::Var(var), ty) | (ty, Type::Var(var)) => {
            // Check for occurs check to prevent infinite types
            if occurs_in(var, &ty) {
                Err(TypeError::occurs_check_failed(var, ty))
            } else {
                Ok(Subst::unit(var, ty))
            }
        }

        // Function types unify if their components unify
        (Type::Fn(a1, b1), Type::Fn(a2, b2)) => {
            let s1 = unify(*a1, *a2)?;
            let s2 = unify(s1.apply(*b1), s1.apply(*b2))?;
            Ok(s2.compose(s1))
        }

        // Tuple types unify if they have the same length and their components unify
        (Type::Tuple(items1), Type::Tuple(items2)) if items1.len() == items2.len() => items1
            .into_iter()
            .zip(items2)
            .try_fold(Subst::empty(), |acc, (t1, t2)| {
                let s = unify(acc.apply(t1), acc.apply(t2))?;
                Ok(s.compose(acc))
            }),

        // All other cases fail to unify
        (ty1, ty2) => Err(TypeError::type_mismatch(ty1, ty2)),
    }
}
