use std::fmt;
use std::ops::Range;
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
    ColonyDecl {
        name: String,
        resources: Vec<ExprAST>,
        rules: Vec<RuleSetAST>,
        location: Range<usize>,
    },
    ColonyExtension {
        name: String,
        resources: Vec<ExprAST>,
        rules: Vec<RuleSetAST>,
        location: Range<usize>,
    },
}

impl fmt::Debug for StatementAST {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            StatementAST::ColonyDecl { name, resources, rules, location: _ } =>
                write!(f, "{{\"ColonyDecl({})\":{{\".resources\":{:?},\".rules\":{:?}}}}}", name, resources, rules),
            StatementAST::ColonyExtension { name, resources, rules, location: _ } =>
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
    /// 連続規則の RuleAST::conditions での開始インデックス
    pub sqc_start: usize,
    /// 連続規則の RuleAST::conditions での終了インデックス
    pub sqc_end: usize,
    /// プログラム中の式の位置
    pub location: Range<usize>,
    /// 意味解析で追加されるメタデータ
    pub meta: Option<semantics::ConditionASTMeta>,
}

pub struct OutputAST {
    pub expr: ExprAST,
    /// 出力先コロニーの名前
    pub destination: Option<String>,
    /// プログラム中の式の位置
    pub location: Range<usize>,
    /// 意味解析で追加されるメタデータ
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
    Double(f64),
    Str(String),
    Bool(bool),
    Capture(String, Range<usize>),
    UnaryOp(UnaryOpcode, Box<ExprAST>, Range<usize>),
    BinaryOp(Box<ExprAST>, Opcode, Box<ExprAST>, Range<usize>),
}

impl fmt::Debug for ExprAST {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ExprAST::Number(n) =>
                write!(f, "{{\"Number({})\":{{\"_\":{{}}}}}}", n),
            ExprAST::Double(n) =>
                write!(f, "{{\"Double({})\":{{\"_\":{{}}}}}}", n),
            ExprAST::Str(s) =>
                write!(f, "{{\"Str({})\":{{\"_\":{{}}}}}}", urlencode(s)),
            ExprAST::Bool(b) =>
                write!(f, "{{\"Bool({})\":{{\"_\":{{}}}}}}", b),
            ExprAST::Capture(s, _) =>
                write!(f, "{{\"Capture({})\":{{\"_\":{{}}}}}}", s),
            ExprAST::UnaryOp(op, operand, _) =>
                write!(f, "{{\"BinaryOp({:?})\":{:?}}}", op, operand),
            ExprAST::BinaryOp(lhs, op, rhs, _) =>
                write!(f, "{{\"BinaryOp({:?})\":{{\".lhs\":{:?},\".rhs\":{:?}}}}}", op, lhs, rhs),
        }
    }
}

// MARK: Opcode

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOpcode {
    Neg,
    LogicalNot,
    BitNot,
    As(semantics::Type),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Opcode {
    Mul,
    Div,
    Mod,
    Add,
    Sub,
    BitAnd,
    BitOr,
    BitXor,
    BitShiftLeft,
    BitShiftRight,
    LogicalAnd,
    LogicalOr,
    Eq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,
}
