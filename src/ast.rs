use std::fmt;
use regex::Regex;

/// URL エンコード
fn urlencode(s: &str) -> String {
    let re = Regex::new(r#"[()"\\%\x00-\x1F]"#).unwrap();
    re.replace_all(s, |caps: &regex::Captures| {
        let c = caps[0].chars().next().unwrap();
        format!("%{:02X}", c as u32)
    }).into_owned()
}

// MARK: StatementAST

pub enum StatementAST {
    ColonyDecl { name: String, resources: Vec<ExprAST>, rules: Vec<RuleSetAST> },
    ColonyExtension { name: String, resources: Vec<ExprAST>, rules: Vec<RuleSetAST> },
}

impl fmt::Debug for StatementAST {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            StatementAST::ColonyDecl { name, resources, rules } =>
                write!(f, "{{\"ColonyDecl({})\":{{\".resources\":{:?},\".rules\":{:?}}}}}", name, resources, rules),
            StatementAST::ColonyExtension { name, resources, rules } =>
                write!(f, "{{\"ColonyExtension({})\":{{\".resources\":{:?},\".rules\":{:?}}}}}", name, resources, rules),
        }
    }
}

// MARK: RuleSetAST

pub struct RuleSetAST {
    pub rules: Vec<RuleAST>,
}

impl fmt::Debug for RuleSetAST {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{{\"RuleSet\":{{\".rules\": {:?}}}}}", self.rules)
    }
}

// MARK: RuleAST

pub struct RuleAST {
    pub conditions: Vec<ExprAST>,
    pub destination: Option<String>,
    pub outputs: Vec<ExprAST>,
}

impl fmt::Debug for RuleAST {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(destination) = &self.destination {
            write!(f, "{{\"Rule\":{{\".conditions\":{:?},\".destination\":{:?},\".outputs\":{:?}}}}}", self.conditions, destination, self.outputs)
        } else {
            write!(f, "{{\"Rule\":{{\".conditions\":{:?},\".outputs\":{:?}}}}}", self.conditions, self.outputs)
        }
    }
}

// MARK: ExprAST

pub enum ExprAST {
    Number(i32),
    Str(String),
    Capture(String),
    BinaryOp(Box<ExprAST>, Opcode, Box<ExprAST>),
}

impl fmt::Debug for ExprAST {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ExprAST::Number(n) =>
                write!(f, "{{\"Number({})\":{{\"_\":{{}}}}}}", n),
            ExprAST::Str(s) =>
                write!(f, "{{\"Str({})\":{{\"_\":{{}}}}}}", urlencode(s)),
            ExprAST::Capture(s) =>
                write!(f, "{{\"Capture({})\":{{\"_\":{{}}}}}}", s),
            ExprAST::BinaryOp(lhs, op, rhs) =>
                write!(f, "{{\"BinaryOp({:?})\":{{\".lhs\":{:?},\".rhs\":{:?}}}}}", op, lhs, rhs),
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
