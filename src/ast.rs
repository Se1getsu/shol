use std::fmt;

// MARK: Statement

pub enum Statement {
    ColonyDecl { name: String, resources: Vec<Expr>, rules: Vec<RuleSet> },
    ColonyExtension { name: String, resources: Vec<Expr>, rules: Vec<RuleSet> },
}

impl fmt::Debug for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Statement::ColonyDecl { name, resources, rules } =>
                write!(f, "{{\"ColonyDecl({})\": {{\".resources\":{:?},\".rules\":{:?}}}}}", name, resources, rules),
            Statement::ColonyExtension { name, resources, rules } =>
                write!(f, "{{\"ColonyExtension({})\": {{\".resources\":{:?},\".rules\":{:?}}}}}", name, resources, rules),
        }
    }
}

// MARK: RuleSet

pub enum RuleSet {
    Rules(Vec<Rule>),
}

impl fmt::Debug for RuleSet {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RuleSet::Rules(rules) =>
                write!(f, "{{\"Rules\": {{\".rules\": {:?}}}}}", rules),
        }
    }
}

// MARK: Rule

pub enum Rule {
    Rule { conditions: Vec<Expr>, destination: Option<String>, outputs: Vec<Expr> },
}

impl fmt::Debug for Rule {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Rule::Rule { conditions, destination, outputs } => {
                if let Some(destination) = destination {
                    write!(f, "{{\"Rule\": {{\".conditions\":{:?},\".destination\":{:?},\".outputs\":{:?}}}}}", conditions, destination, outputs)
                } else {
                    write!(f, "{{\"Rule\": {{\".conditions\":{:?},\".outputs\":{:?}}}}}", conditions, outputs)
                }
            }
        }
    }
}

// MARK: Expr

pub enum Expr {
    Number(i32),
    Str(String),
    Capture(String),
    BinaryOp(Box<Expr>, Opcode, Box<Expr>),
}

impl fmt::Debug for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::Number(n) =>
                write!(f, "{{\"Number({})\": {{}}}}", n),
            Expr::Str(s) =>
                write!(f, "{{\"Str({})\": {{}}}}", s),
            Expr::Capture(s) =>
                write!(f, "{{\"Capture({})\": {{}}}}", s),
            Expr::BinaryOp(lhs, op, rhs) =>
                write!(f, "{{\"BinaryOp({:?})\": {{\".lhs\":{:?},\".rhs\":{:?}}}}}", op, lhs, rhs),
        }
    }
}

// MARK: Opcode

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
