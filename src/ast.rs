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

// MARK: Statement

pub enum Statement {
    ColonyDecl { name: String, resources: Vec<Expr>, rules: Vec<RuleSet> },
    ColonyExtension { name: String, resources: Vec<Expr>, rules: Vec<RuleSet> },
}

impl fmt::Debug for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Statement::ColonyDecl { name, resources, rules } =>
                write!(f, "{{\"ColonyDecl({})\":{{\".resources\":{:?},\".rules\":{:?}}}}}", name, resources, rules),
            Statement::ColonyExtension { name, resources, rules } =>
                write!(f, "{{\"ColonyExtension({})\":{{\".resources\":{:?},\".rules\":{:?}}}}}", name, resources, rules),
        }
    }
}

// MARK: RuleSet

pub struct RuleSet {
    pub rules: Vec<Rule>,
}

impl fmt::Debug for RuleSet {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{{\"RuleSet\":{{\".rules\": {:?}}}}}", self.rules)
    }
}

// MARK: Rule

pub struct Rule {
    pub conditions: Vec<Expr>,
    pub destination: Option<String>,
    pub outputs: Vec<Expr>,
}

impl fmt::Debug for Rule {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(destination) = &self.destination {
            write!(f, "{{\"Rule\":{{\".conditions\":{:?},\".destination\":{:?},\".outputs\":{:?}}}}}", self.conditions, destination, self.outputs)
        } else {
            write!(f, "{{\"Rule\":{{\".conditions\":{:?},\".outputs\":{:?}}}}}", self.conditions, self.outputs)
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
                write!(f, "{{\"Number({})\":{{\"_\":{{}}}}}}", n),
            Expr::Str(s) =>
                write!(f, "{{\"Str({})\":{{\"_\":{{}}}}}}", urlencode(s)),
            Expr::Capture(s) =>
                write!(f, "{{\"Capture({})\":{{\"_\":{{}}}}}}", s),
            Expr::BinaryOp(lhs, op, rhs) =>
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
