use super::VmValue;

#[derive(Clone, Debug, PartialEq)]
pub enum Instruction {
    // Stack operations
    LoadConst(VmValue),
    LoadVar(String),
    StoreVar(String),

    // Function operations
    MakeClosure { arg_name: String, body_len: usize },
    Call,
    Return,

    // Tuple operations
    MakeTuple(usize), // number of elements

    // Binary operations (native implementations)
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Lt,
    Gt,
    And,
    Or,
    Not,
}
