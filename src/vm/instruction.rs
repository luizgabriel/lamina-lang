use super::VmValue;

#[derive(Clone, Debug, PartialEq)]
pub enum Instruction {
    // Stack operations
    LoadConst(VmValue),
    LoadVar(String),
    StoreVar(String),

    // Function operations
    MakeRecursiveClosure {
        fn_name: String,
        arg_name: String,
        body_len: usize,
    },
    MakeClosure {
        arg_name: String,
        body_len: usize,
    },
    Call,
    Return,

    // Tuple operations
    MakeTuple(usize), // number of elements

    // Control flow
    Jump(usize),        // Jump forward by n instructions
    JumpIfFalse(usize), // Jump forward by n instructions if top of stack is false

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
