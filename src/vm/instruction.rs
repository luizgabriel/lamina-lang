use super::VmValue;

#[derive(Clone, Debug, PartialEq)]
pub enum Instruction {
    // Stack operations
    LoadConst(VmValue),
    LoadVar(String),
    StoreVar(String),

    // Function operations
    MakeClosure {
        fn_name: Option<String>, // Some(name) for recursive functions, None for regular closures
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
