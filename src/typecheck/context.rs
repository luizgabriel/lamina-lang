use crate::typecheck::TypeVar;

#[derive(Default, Debug, Clone)]
pub struct TypeVarContext {
    next_id: usize,
}

impl TypeVarContext {
    pub fn fresh(&mut self) -> TypeVar {
        let var = TypeVar(self.next_id);
        self.next_id += 1;
        var
    }
}
