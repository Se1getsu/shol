use std::collections::{HashMap, HashSet};
use std::fmt::{self, Debug};
use crate::ast::{self, Opcode};

// MARK: メタデータ

pub struct RuleASTMeta {
    pub captures: HashMap<String, TypeHint>,
    pub condition_kinds: Vec<ConditionKind>,
}

impl Debug for RuleASTMeta {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{{\".condition_kinds\":{:?}}}", self.condition_kinds)
    }
}

/// キャプチャの型ヒント
#[derive(Debug)]
pub struct TypeHint {
    pub possible_types: HashSet<Type>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Type {
    // NOTE: 変更時には Type::all_types() にも変更を反映すること
    Int,
    String,
    Bool,
}

/// 条件式種別
#[derive(Clone)]
pub enum ConditionKind {
    /// 条件式の値と一致するリソースを指定している
    Equal(Type),
    /// キャプチャ単体
    Capture(String),
    /// キャプチャを含む条件式 (条件式の型とキャプチャ名)
    CaptureCondition(String),
}

impl Debug for ConditionKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ConditionKind::Equal(t) =>
                write!(f, "\"Equal({:?})\"", t),
            ConditionKind::Capture(s) =>
                write!(f, "\"Capture({})\"", s),
            ConditionKind::CaptureCondition(s) =>
                write!(f, "\"CaptureCondition({})\"", s),
        }
    }
}

// MARK: 型推論に関するもの

impl Type {
    pub fn all_types() -> HashSet<Type> {
        let mut types = HashSet::new();
        types.insert(Type::Int);
        types.insert(Type::String);
        types.insert(Type::Bool);
        types
    }
}

/// オペレータの型シグネチャ
struct OpcodeSignature {
    lhs: Type,
    rhs: Type,
    result: Type,
}

impl OpcodeSignature {
    /// 指定したオペレータの型シグネチャを返す
    fn get_signatures(opcode: Opcode) -> Vec<Self> {
        match opcode {
            Opcode::Add => vec![
                Self { lhs: Type::Int, rhs: Type::Int, result: Type::Int },
                Self { lhs: Type::String, rhs: Type::String, result: Type::String },
            ],
            Opcode::Sub | Opcode::Mul | Opcode::Div | Opcode::Mod => vec![
                Self { lhs: Type::Int, rhs: Type::Int, result: Type::Int },
            ],
            Opcode::Eq | Opcode::Ne => vec![
                Self { lhs: Type::Int, rhs: Type::Int, result: Type::Int },
                Self { lhs: Type::String, rhs: Type::String, result: Type::Int },
            ],
            Opcode::Lt | Opcode::Le | Opcode::Gt | Opcode::Ge => vec![
                Self { lhs: Type::Int, rhs: Type::Int, result: Type::Int },
            ],
        }
    }
}

impl Opcode {
    /// 演算結果の型を返す
    fn result_type(self, lhs: Type, rhs: Type) -> Option<Type> {
        OpcodeSignature::get_signatures(self).iter().find_map(|sig| {
            if sig.lhs == lhs && sig.rhs == rhs {
                Some(sig.result)
            } else {
                None
            }
        })
    }
}

// MARK: AST 探索関数

// 子ノードを探索するだけ
pub fn analyze_program(program: &mut Vec<ast::StatementAST>) {
    program.iter_mut().for_each(analyze_statement);
}

// 子ノードを探索するだけ
fn analyze_statement(statement: &mut ast::StatementAST) {
    match statement {
        ast::StatementAST::ColonyDecl { name: _, resources: _, rules } =>
            rules.iter_mut().for_each(analyze_rule_set),
        ast::StatementAST::ColonyExtension { name: _, resources: _, rules } =>
            rules.iter_mut().for_each(analyze_rule_set),
    }
}

// 子ノードを探索するだけ
fn analyze_rule_set(rule_set: &mut ast::RuleSetAST) {
    rule_set.rules.iter_mut().for_each(analyze_rule);
}

// RuleAST のメタデータを作成
fn analyze_rule(rule: &mut ast::RuleAST) {
    // メタデータ構造体を作成
    let mut meta = RuleASTMeta {
        captures: HashMap::new(),
        condition_kinds: Vec::new(),
    };

    // 条件式を解析
    for condition in rule.conditions.iter() {
        let kind = condition_kind(condition);
        match &kind {
            ConditionKind::Equal(_) => {
                meta.condition_kinds.push(kind);
            }
            ConditionKind::Capture(name) => {
                if meta.captures.contains_key(name) {
                    panic!("別々の条件に同じ名前のキャプチャが使われています: {}", name);
                }
                meta.captures.insert(name.clone(), TypeHint { possible_types: Type::all_types() });
                meta.condition_kinds.push(kind);
            },
            ConditionKind::CaptureCondition(name) => {
                if meta.captures.contains_key(name) {
                    panic!("別々の条件に同じ名前のキャプチャが使われています: {}", name);
                }
                // TODO: possible_types の型推論を行う
                meta.captures.insert(name.clone(), TypeHint { possible_types: Type::all_types() });
                meta.condition_kinds.push(kind);
            },
        }
    }

    for _output in rule.outputs.iter() {
        // TODO: 出力式の型推論を行う
    }
    rule.meta = Some(meta);
}

// 条件式の型を推論する
fn condition_kind(expr: &ast::ExprAST) -> ConditionKind {
    match expr {
        ast::ExprAST::Number(_) => ConditionKind::Equal(Type::Int),
        ast::ExprAST::Str(_) => ConditionKind::Equal(Type::String),
        ast::ExprAST::Capture(name) => ConditionKind::Capture(name.clone()),
        ast::ExprAST::BinaryOp(lhs, opcode, rhs) => {
            // 左辺と右辺の条件式種別を取得
            let lhs_kind = condition_kind(lhs);
            let rhs_kind = condition_kind(rhs);

            // 左辺と右辺のどちらかがキャプチャ条件式の場合, キャプチャ条件式を返す
            match &lhs_kind {
                ConditionKind::Equal(t) => {
                    match &rhs_kind {
                        ConditionKind::Equal(t_r) => {
                            if let Some(result) = opcode.result_type(*t, *t_r) {
                                return ConditionKind::Equal(result);
                            } else {
                                panic!("不正な型の演算です: {:?} {:?} {:?}", t, opcode, t_r);
                            }
                        },
                        ConditionKind::Capture(name) => {
                            return ConditionKind::CaptureCondition((*name).clone());
                        },
                        ConditionKind::CaptureCondition(_) => {
                            return rhs_kind;
                        },
                    }
                },
                ConditionKind::Capture(name) => {
                    match &rhs_kind {
                        ConditionKind::Equal(_) => {
                            return ConditionKind::CaptureCondition((*name).clone());
                        },
                        ConditionKind::Capture(name_r) => {
                            if name != name_r {
                                panic!("1 つの条件に複数のキャプチャが存在します: {}, {}", name, name_r);
                            }
                            return ConditionKind::CaptureCondition((*name).clone());
                        },
                        ConditionKind::CaptureCondition(name_r) => {
                            if name != name_r {
                                panic!("1 つの条件に複数のキャプチャが存在します: {}, {}", name, name_r);
                            }
                            return rhs_kind;
                        },
                    }
                }
                ConditionKind::CaptureCondition(name) => {
                    match &rhs_kind {
                        ConditionKind::Equal(_) => {
                            return lhs_kind;
                        },
                        ConditionKind::Capture(name_r) => {
                            if name != name_r {
                                panic!("1 つの条件に複数のキャプチャが存在します: {}, {}", name, name_r);
                            }
                            return lhs_kind;
                        },
                        ConditionKind::CaptureCondition(name_r) => {
                            if name != name_r {
                                panic!("1 つの条件に複数のキャプチャが存在します: {}, {}", name, name_r);
                            }
                            return lhs_kind;
                        },
                    }
                },
            }
        },
    }
}
