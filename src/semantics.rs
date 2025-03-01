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
    /// キャプチャを含む条件式 (キャプチャ名)
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
                Self { lhs: Type::String, rhs: Type::Int, result: Type::String },
                Self { lhs: Type::String, rhs: Type::Bool, result: Type::String },
            ],
            Opcode::Sub | Opcode::Mul | Opcode::Div | Opcode::Mod => vec![
                Self { lhs: Type::Int, rhs: Type::Int, result: Type::Int },
            ],
            Opcode::Eq | Opcode::Ne => vec![
                Self { lhs: Type::Int, rhs: Type::Int, result: Type::Bool },
                Self { lhs: Type::String, rhs: Type::String, result: Type::Bool },
                Self { lhs: Type::Bool, rhs: Type::Bool, result: Type::Bool },
            ],
            Opcode::Lt | Opcode::Le | Opcode::Gt | Opcode::Ge => vec![
                Self { lhs: Type::Int, rhs: Type::Int, result: Type::Bool },
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
                let types = analyze_condition(condition);
                meta.captures.insert(name.clone(), TypeHint { possible_types: types });
                meta.condition_kinds.push(kind);
            },
        }
    }

    // 出力式を解析
    analyze_output(&rule.outputs, &mut meta.captures);

    // AST にメタデータを追加
    rule.meta = Some(meta);
}

// MARK: 条件式の型推論

/// 条件式の種別を取得
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

/// 条件式の型推論をする
/// 戻り値: キャプチャの possible_types
fn analyze_condition(expr: &ast::ExprAST) -> HashSet<Type> {
    let mut possible_types = HashSet::new();
    Type::all_types().into_iter().for_each(|t| {
        let result = analyze_condition_ast(expr, t);
        if let Ok(_) = result {
            possible_types.insert(t);
        }
    });
    if possible_types.is_empty() {
        panic!("この条件式を計算できるキャプチャ型は存在しません: {:?}", expr);
    }
    println!("possible_types: {:?}", possible_types);
    possible_types
}

/// 条件式の型推論のために AST を探索する
/// 戻り値
/// - Ok: キャプチャの型を capture_type とした時の式の型
/// - Err: 型エラー
fn analyze_condition_ast(expr: &ast::ExprAST, capture_type: Type) -> Result<Type, ()> {
    match expr {
        ast::ExprAST::Number(_) => Ok(Type::Int),
        ast::ExprAST::Str(_) => Ok(Type::String),
        ast::ExprAST::Capture(_) => Ok(capture_type),
        ast::ExprAST::BinaryOp(lhs, opcode, rhs) => {
            let lhs_type = analyze_condition_ast(lhs, capture_type);
            let rhs_type = analyze_condition_ast(rhs, capture_type);
            match (lhs_type, rhs_type) {
                (Ok(lhs_type), Ok(rhs_type)) => {
                    if let Some(result) = opcode.result_type(lhs_type, rhs_type) {
                        Ok(result)
                    } else {
                        Err(())
                    }
                },
                _ => Err(()),
            }
        },
    }
}

// MARK: 出力式の型推論

/// infers 配列のインデックス
#[derive(Clone, Copy)]
struct InfersIndex(usize);

impl Debug for InfersIndex {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// 推論された型
#[derive(Debug, Clone)]
enum InferredType {
    /// 定数式
    Constant(Type),
    /// キャプチャ (possible_types に対応する型)
    Capture(String),
    /// キャプチャを含む二項演算式
    BinaryExpr {
        opcode: Opcode,
        lhs: InfersIndex,
        rhs: InfersIndex,
        parent: Option<InfersIndex>,
        result: HashSet<Type>,
        needs_update: bool,
    },
}

impl InferredType {
    // 親を設定
    fn set_parent(&mut self, parent: &InfersIndex) {
        match self {
            InferredType::BinaryExpr { parent: p, .. } => {
                *p = Some(*parent);
            },
            _ => {},
        }
    }
}

/// 出力式の型推論をする
fn analyze_output(expr: &Vec<ast::ExprAST>, captures: &mut HashMap<String, TypeHint>) {
    // InferredType を保存するベクタ
    let mut infers = build_infers(expr, captures);
    println!("built infers: {}", fmt_infers(&infers));
}

/// infers 配列のデバッグ用文字列
fn fmt_infers(infers: &Vec<InferredType>) -> String {
    let mut s = String::from("[\n");
    for (i, infer) in infers.iter().enumerate() {
        s.push_str(&format!("{:>4}: {:?}\n", i, infer));
    }
    s.push_str("]");
    s
}

/// infers 配列を作成する
fn build_infers(expr: &Vec<ast::ExprAST>, captures: &HashMap<String, TypeHint>) -> Vec<InferredType> {
    // InferredType を保存するベクタ
    let mut infers: Vec<InferredType> = captures.keys()
        .map(|name| {
            InferredType::Capture(name.clone())
        }).collect();

    // キャプチャ名とインデックスの対応表
    let capture_infers: HashMap<String, usize> = captures.keys()
        .enumerate()
        .map(|(i, name)| (name.clone(), i))
        .collect();

    // AST を探索して infers 配列を作成
    expr.iter().for_each(|e| {
        analyze_output_ast(e, &mut infers, &capture_infers);
    });
    infers
}

/// analyze_output_ast 関数の戻り値
enum AOAResult {
    Constant(Type),
    Infer { index: InfersIndex },
}

// infers 配列作成のために AST を探索する
fn analyze_output_ast(
    expr: &ast::ExprAST,
    infers: &mut Vec<InferredType>,
    capture_infers: &HashMap<String, usize>,
) -> AOAResult {
    match expr {
        // 定数式の型を返す
        ast::ExprAST::Number(_) => AOAResult::Constant(Type::Int),
        ast::ExprAST::Str(_) => AOAResult::Constant(Type::String),

        // capture_infers に登録されているインデックスを返す
        ast::ExprAST::Capture(name) => {
            if let Some(index) = capture_infers.get(name) {
                AOAResult::Infer { index: InfersIndex(*index) }
            } else {
                panic!("未定義のキャプチャ: {}", name);
            }
        },

        // 自身を infers に追加し, そのインデックスを返す
        ast::ExprAST::BinaryOp(lhs, opcode, rhs) => {
            let lhs = analyze_output_ast(lhs, infers, capture_infers);
            let rhs = analyze_output_ast(rhs, infers, capture_infers);

            // 定数式なら AOAResult::Constant を返す
            if let (AOAResult::Constant(t_l), AOAResult::Constant(t_r)) = (&lhs, &rhs) {
                if let Some(result) = opcode.result_type(*t_l, *t_r) {
                    return AOAResult::Constant(result);
                } else {
                    panic!("不正な型の演算です: {:?} {:?} {:?}", t_l, opcode, t_r);
                }
            }

            // lhs または rhs が定数式なら infers に追加してインデックスを作成
            let i_l = match &lhs {
                AOAResult::Infer { index } => *index,
                AOAResult::Constant(t) => {
                    infers.push(InferredType::Constant(*t));
                    InfersIndex(infers.len() - 1)
                }
            };
            let i_r = match &rhs {
                AOAResult::Infer { index } => *index,
                AOAResult::Constant(t) => {
                    infers.push(InferredType::Constant(*t));
                    InfersIndex(infers.len() - 1)
                }
            };

            // 自身を infer に追加する
            infers.push(InferredType::BinaryExpr {
                opcode: *opcode,
                lhs: i_l,
                rhs: i_r,
                parent: None,
                result: Type::all_types(),
                needs_update: true,
            });
            let i_self = InfersIndex(infers.len() - 1);

            // 自身を lhs, rhs の親ノードを設定
            infers[i_l.0].set_parent(&i_self);
            infers[i_r.0].set_parent(&i_self);

            AOAResult::Infer { index: i_self }
        },
    }
}
