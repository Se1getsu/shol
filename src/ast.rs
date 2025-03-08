use std::fmt;
use regex::Regex;

use crate::semantics;

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

// MARK: RuleSetAST, RuleAST

pub struct RuleSetAST {
    pub rules: Vec<RuleAST>,
}

pub struct RuleAST {
    pub conditions: Vec<ConditionAST>,
    pub outputs: Vec<OutputAST>,
    pub meta: Option<semantics::RuleASTMeta>,
}

impl fmt::Debug for RuleSetAST {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{{\"RuleSet\":{{\".rules\": {:?}}}}}", self.rules)
    }
}

impl fmt::Debug for RuleAST {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{{\"Rule\":{{\".conditions\":{:?},\".outputs\":{:?}}}}}",
            self.conditions,
            self.outputs,
        )
    }
}

// MARK: ConditionAST, OutputAST

pub struct ConditionAST {
    pub expr: ExprAST,
    pub meta: Option<semantics::ConditionASTMeta>,
}

pub struct OutputAST {
    pub expr: ExprAST,
    pub destination: Option<String>,
    pub meta: Option<semantics::OutputASTMeta>,
}

impl fmt::Debug for ConditionAST {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{{\"Condition{}\":{{\".expr\":{:?}}}}}",
            self.meta.as_ref().map(|m| format!(":{:?}", m.kind)).unwrap_or_default(),
            self.expr
        )
    }
}

impl fmt::Debug for OutputAST {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{{\"Output({}){}\":{{\".expr\":{:?}}}}}",
            self.meta.as_ref().map(|m| format!("{}", m.associated_captures.join(","))).unwrap_or_default(),
            self.destination.as_ref().map_or("#".to_string(), |d| format!("#{}", d)),
            self.expr
        )
    }
}

// MARK: ExprAST

pub enum ExprAST {
    Number(i32),
    Str(String),
    Bool(bool),
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
            ExprAST::Bool(b) =>
                write!(f, "{{\"Bool({})\":{{\"_\":{{}}}}}}", b),
            ExprAST::Capture(s) =>
                write!(f, "{{\"Capture({})\":{{\"_\":{{}}}}}}", s),
            ExprAST::BinaryOp(lhs, op, rhs) =>
                write!(f, "{{\"BinaryOp({:?})\":{{\".lhs\":{:?},\".rhs\":{:?}}}}}", op, lhs, rhs),
        }
    }
}

// MARK: Opcode

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
