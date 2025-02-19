#[derive(Debug)]
pub enum Statement {
    ColonyDecl { name: String, resources: Vec<Resource>, rules: Vec<RuleSet> },
    ColonyExtension { name: String, resources: Vec<Resource>, rules: Vec<RuleSet> },
}

#[derive(Debug)]
pub enum Resource {
    Number(i32),
    Str(String),
}

#[derive(Debug)]
pub enum RuleSet {
    Rules(Vec<Rule>),
}

#[derive(Debug)]
pub enum Rule {
    Rule { conditions: Vec<Expr>, destination: Option<String>, outputs: Vec<Expr> },
}

#[derive(Debug)]
pub enum Expr {
    Number(i32),
    Str(String),
    Capture(String),
    BinaryOp(Box<Expr>, Opcode, Box<Expr>),
}

#[derive(Debug)]
pub enum Opcode {
    Mul,
    Div,
    Mod,
    Add,
    Sub,
    Eq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,
}
