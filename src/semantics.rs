use std::collections::{HashMap, HashSet};
use std::fmt::{self, Debug};
use std::ops::Range;
use crate::ast::{self, Opcode, ProgramAST, UnaryOpcode};
use crate::semantic_error::SemanticError;

// MARK: メタデータ

pub struct ProgramASTMeta {
    /// コロニー名と V_COLONIES のインデックスの対応表
    pub colony_indices: HashMap<String, usize>,
}

pub struct RuleASTMeta {
    pub captures: HashMap<String, TypeHint>,
}

pub struct ConditionASTMeta {
    pub kind: ConditionKind,
}

pub struct OutputASTMeta {
    /// 出力式に含まれるキャプチャのリスト
    pub associated_captures: Vec<String>,
}

/// キャプチャの型ヒント
#[derive(Debug, Clone)]
pub struct TypeHint {
    pub possible_types: HashSet<Type>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Type {
    // NOTE: 変更時には Type::all_types() にも変更を反映すること
    Int,
    Double,
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
                write!(f, "値({:?})", t),
            ConditionKind::Capture(caputure_name) =>
                write!(f, "ｷｬﾌﾟﾁｬ({})", caputure_name),
            ConditionKind::CaptureCondition(caputure_name) =>
                write!(f, "ｷｬﾌﾟﾁｬ条件式({})", caputure_name),
        }
    }
}

// MARK: 型推論に関するもの

impl Type {
    pub fn all_types() -> HashSet<Type> {
        let mut types = HashSet::new();
        types.insert(Type::Int);
        types.insert(Type::Double);
        types.insert(Type::String);
        types.insert(Type::Bool);
        types
    }
}

/// オペレータの型シグネチャ
struct UnaryOpcodeSignature {
    operand: Type,
    result: Type,
}
struct OpcodeSignature {
    lhs: Type,
    rhs: Type,
    result: Type,
}

impl UnaryOpcodeSignature {
    /// 指定したオペレータの型シグネチャを返す
    fn get_signatures(opcode: UnaryOpcode) -> Vec<Self> {
        match opcode {
            UnaryOpcode::Neg => vec![
                Self { operand: Type::Int, result: Type::Int },
                Self { operand: Type::Double, result: Type::Double },
            ],
            UnaryOpcode::LogicalNot => vec![
                Self { operand: Type::Bool, result: Type::Bool },
            ],
            UnaryOpcode::BitNot => vec![
                Self { operand: Type::Int, result: Type::Int },
            ],
            UnaryOpcode::As(t) => vec![
                Self { operand: t, result: t },
            ],
        }
    }
}
impl OpcodeSignature {
    /// 指定したオペレータの型シグネチャを返す
    fn get_signatures(opcode: Opcode) -> Vec<Self> {
        match opcode {
            Opcode::Add => vec![
                Self { lhs: Type::Int, rhs: Type::Int, result: Type::Int },
                Self { lhs: Type::Int, rhs: Type::Double, result: Type::Double },
                Self { lhs: Type::Double, rhs: Type::Int, result: Type::Double },
                Self { lhs: Type::Double, rhs: Type::Double, result: Type::Double },
                Self { lhs: Type::String, rhs: Type::String, result: Type::String },
                Self { lhs: Type::String, rhs: Type::Int, result: Type::String },
                Self { lhs: Type::String, rhs: Type::Double, result: Type::String },
                Self { lhs: Type::String, rhs: Type::Bool, result: Type::String },
            ],
            Opcode::Sub | Opcode::Mul | Opcode::Div | Opcode::Mod => vec![
                Self { lhs: Type::Int, rhs: Type::Int, result: Type::Int },
                Self { lhs: Type::Int, rhs: Type::Double, result: Type::Double },
                Self { lhs: Type::Double, rhs: Type::Int, result: Type::Double },
                Self { lhs: Type::Double, rhs: Type::Double, result: Type::Double },
            ],
            Opcode::BitAnd | Opcode::BitOr => vec![
                Self { lhs: Type::Int, rhs: Type::Int, result: Type::Int },
                Self { lhs: Type::Bool, rhs: Type::Bool, result: Type::Bool },
            ],
            Opcode::BitXor | Opcode::BitShiftLeft | Opcode::BitShiftRight => vec![
                Self { lhs: Type::Int, rhs: Type::Int, result: Type::Int },
            ],
            Opcode::LogicalAnd | Opcode::LogicalOr => vec![
                Self { lhs: Type::Bool, rhs: Type::Bool, result: Type::Bool },
            ],
            Opcode::Eq | Opcode::Ne => vec![
                Self { lhs: Type::Int, rhs: Type::Int, result: Type::Bool },
                Self { lhs: Type::Int, rhs: Type::Double, result: Type::Bool },
                Self { lhs: Type::Double, rhs: Type::Int, result: Type::Bool },
                Self { lhs: Type::Double, rhs: Type::Double, result: Type::Bool },
                Self { lhs: Type::String, rhs: Type::String, result: Type::Bool },
                Self { lhs: Type::Bool, rhs: Type::Bool, result: Type::Bool },
            ],
            Opcode::Lt | Opcode::Le | Opcode::Gt | Opcode::Ge => vec![
                Self { lhs: Type::Int, rhs: Type::Int, result: Type::Bool },
                Self { lhs: Type::Int, rhs: Type::Double, result: Type::Bool },
                Self { lhs: Type::Double, rhs: Type::Int, result: Type::Bool },
                Self { lhs: Type::Double, rhs: Type::Double, result: Type::Bool },
            ],
        }
    }
}

pub trait UnaryOpcodeSignatureExt {
    fn result_type(self, operand: Type) -> Option<Type>;
}
pub trait OpcodeSignatureExt {
    fn result_type(self, lhs: Type, rhs: Type) -> Option<Type>;
}

impl UnaryOpcodeSignatureExt for UnaryOpcode {
    /// 演算結果の型を返す
    fn result_type(self, operand: Type) -> Option<Type> {
        UnaryOpcodeSignature::get_signatures(self).iter().find_map(|sig| {
            if sig.operand == operand {
                Some(sig.result)
            } else {
                None
            }
        })
    }
}
impl OpcodeSignatureExt for Opcode {
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

/// 式の型検証
/// - Ok: 式の型を返す
/// - Err: 型エラーが発生したノード
///
/// NOTE: 式に登場するキャプチャの型は captures に登録されている必要がある
fn type_validate_expr<'a>(expr: &'a ast::ExprAST, captures: &HashMap<String, Type>) -> Result<Type, &'a ast::ExprAST> {
    match expr {
        ast::ExprAST::Number(_) => Ok(Type::Int),
        ast::ExprAST::Double(_) => Ok(Type::Double),
        ast::ExprAST::Str(_) => Ok(Type::String),
        ast::ExprAST::Bool(_) => Ok(Type::Bool),
        ast::ExprAST::Capture(name, _) => Ok(captures.get(name).unwrap().clone()),
        ast::ExprAST::UnaryOp(opcode, operand, _) => {
            let operand_type = type_validate_expr(operand, captures)?;
            if let Some(result) = opcode.result_type(operand_type) {
                Ok(result)
            } else {
                Err(expr)
            }
        },
        ast::ExprAST::BinaryOp(lhs, opcode, rhs, _) => {
            let lhs_type = type_validate_expr(lhs, captures)?;
            let rhs_type = type_validate_expr(rhs, captures)?;
            if let Some(result) = opcode.result_type(lhs_type, rhs_type) {
                Ok(result)
            } else {
                Err(expr)
            }
        },
    }
}

// MARK: AST 探索関数

pub fn analyze_program(program: &mut ProgramAST) -> Result<(), SemanticError> {
    // ProgramAST のメタデータを作成
    let colony_indices: HashMap<String, usize> = {
        let mut colony_indices = HashMap::new();
        let mut locations = HashMap::new();
        let mut count = 0;
        for stmt in &program.statements {
            match stmt {
                ast::StatementAST::ColonyDecl { name, location, .. } |
                ast::StatementAST::ColonyExtension { name, location, .. } => {
                    if colony_indices.contains_key(name) {
                        return Err(SemanticError::duplicate_colony_definition(
                            name.clone(),
                            location,
                            &locations[name],
                        ));
                    }
                    colony_indices.insert(name.clone(), count);
                    locations.insert(name, location.clone());
                    count += 1;
                }
            }
        }
        colony_indices
    };
    program.meta = Some(ProgramASTMeta { colony_indices });

    // 子ノードを探索
    for statement in program.statements.iter_mut() {
        analyze_statement(statement)?;
    }
    Ok(())
}

fn analyze_statement(statement: &mut ast::StatementAST) -> Result<(), SemanticError> {
    // 子ノードを探索
    match statement {
        ast::StatementAST::ColonyDecl { name: _, resources: _, rules, location: _ } => {
            for rule_set in rules.iter_mut() {
                analyze_rule_set(rule_set)?;
            }
        }
        ast::StatementAST::ColonyExtension { name: _, resources: _, rules, location: _ } => {
            for rule_set in rules.iter_mut() {
                analyze_rule_set(rule_set)?;
            }
        }
    }
    Ok(())
}

// 子ノードを探索するだけ
fn analyze_rule_set(rule_set: &mut ast::RuleSetAST) -> Result<(), SemanticError> {
    for rule in rule_set.rules.iter_mut() {
        analyze_rule(rule)?;
    }
    Ok(())
}

// RuleAST のメタデータを作成
fn analyze_rule(rule: &mut ast::RuleAST) -> Result<(), SemanticError> {
    // メタデータ構造体を作成
    let mut meta = RuleASTMeta {
        captures: HashMap::new(),
    };

    // 条件式を解析
    for condition in rule.conditions.iter_mut() {
        analyze_condition(condition, &mut meta)?;
    }

    // 出力式を解析
    analyze_output(&mut rule.outputs, &mut meta.captures)?;

    // AST にメタデータを追加
    rule.meta = Some(meta);
    Ok(())
}

fn analyze_condition(
    condition: &mut ast::ConditionAST,
    rule_meta: &mut RuleASTMeta,
) -> Result<(), SemanticError> {
    // 条件式種別を判定
    let (kind, is_typed_capture) = condition_kind(&condition.expr)?;

    // キャプチャを rule_meta に登録
    match &kind {
        KindWithRange::Equal(_) => (),
        KindWithRange::Capture(name, cap_loc) => {
            if rule_meta.captures.contains_key(name) {
                return Err(SemanticError::duplicate_capture_name(name.clone(), cap_loc));
            }
            if is_typed_capture {
                let types = analyze_capture_condition(
                    &condition.expr,
                    &condition.location,
                    name,
                    is_typed_capture,
                )?;
                rule_meta.captures.insert(name.clone(), TypeHint { possible_types: types });
            } else {
                rule_meta.captures.insert(name.clone(), TypeHint { possible_types: Type::all_types() });
            }
        },
        KindWithRange::CaptureCondition(name, cap_loc) => {
            if rule_meta.captures.contains_key(name) {
                return Err(SemanticError::duplicate_capture_name(name.clone(), cap_loc));
            }
            let types = analyze_capture_condition(
                &condition.expr,
                &condition.location,
                name,
                is_typed_capture,
            )?;
            rule_meta.captures.insert(name.clone(), TypeHint { possible_types: types });
        },
    }

    // AST にメタデータを追加
    condition.meta = Some(ConditionASTMeta { kind: kind.into() });
    Ok(())
}

fn analyze_output(
    outputs: &mut Vec<ast::OutputAST>,
    captures: &mut HashMap<String, TypeHint>
) -> Result<(), SemanticError> {
    // 出力式に登場するキャプチャを調べる
    fn collect_captures(expr: &ast::ExprAST, captures: &mut Vec<String>) {
        match expr {
            ast::ExprAST::Number(_)|ast::ExprAST::Double(_)|ast::ExprAST::Str(_)|ast::ExprAST::Bool(_) => (),
            ast::ExprAST::Capture(name, _) =>
                if !captures.contains(name) { captures.push(name.clone()); },
            ast::ExprAST::UnaryOp(_, operand, _) => {
                collect_captures(operand, captures);
            },
            ast::ExprAST::BinaryOp(lhs, _, rhs, _) => {
                collect_captures(lhs, captures);
                collect_captures(rhs, captures);
            },
        }
    }
    for output in outputs.iter_mut() {
        let mut associated_captures = Vec::new();
        collect_captures(&output.expr, &mut associated_captures);
        output.meta = Some(OutputASTMeta { associated_captures });
    }

    // 型推論に必要な InferredType を保存するベクタ
    let mut infers = build_infers(outputs, captures)?;
    println!("infers 出力式推論前: {}", fmt_infers(&infers, &captures));

    // 型推論
    infer_infers(&mut infers, captures)?;
    println!("infers 推論完了: {}", fmt_infers(&infers, &captures));

    // 型検証
    validate_inference(captures, outputs)?;
    Ok(())
}

// MARK: 条件式の型推論

/// ConditionKind にキャプチャの位置情報を追加した型
#[derive(Clone)]
pub enum KindWithRange {
    /// 条件式の値と一致するリソースを指定している
    Equal(Type),
    /// キャプチャ単体 (キャプチャ名, キャプチャの初登場位置)
    Capture(String, Range<usize>),
    /// キャプチャを含む条件式 (キャプチャ名, キャプチャの初登場位置)
    CaptureCondition(String, Range<usize>),
}

impl From<KindWithRange> for ConditionKind {
    fn from(kind: KindWithRange) -> Self {
        match kind {
            KindWithRange::Equal(t) =>
                ConditionKind::Equal(t),
            KindWithRange::Capture(name, _) =>
                ConditionKind::Capture(name),
            KindWithRange::CaptureCondition(name, _) =>
                ConditionKind::CaptureCondition(name),
        }
    }
}

fn condition_kind(expr: &ast::ExprAST) -> Result<(KindWithRange, bool), SemanticError> {
    let mut kind = _condition_kind(expr)?;
    let mut is_typed_capture = false;

    // $:int のような場合は キャプチャ単体 に変換する
    if let ast::ExprAST::UnaryOp(UnaryOpcode::As(_), operand, _) = expr {
        if let ast::ExprAST::Capture(name, cap_loc) = &**operand {
            kind = KindWithRange::Capture(name.clone(), cap_loc.clone());
            is_typed_capture = true;
        }
    }

    Ok((kind, is_typed_capture))
}

/// 条件式の種別を取得
/// NOTE: $:int のような場合は キャプチャ条件式 と判定する
fn _condition_kind(expr: &ast::ExprAST) -> Result<KindWithRange, SemanticError> {
    match expr {
        ast::ExprAST::Number(_) => Ok(KindWithRange::Equal(Type::Int)),
        ast::ExprAST::Double(_) => Ok(KindWithRange::Equal(Type::Double)),
        ast::ExprAST::Str(_) => Ok(KindWithRange::Equal(Type::String)),
        ast::ExprAST::Bool(_) => Ok(KindWithRange::Equal(Type::Bool)),
        ast::ExprAST::Capture(name, cap_loc) =>
            Ok(KindWithRange::Capture(name.clone(), cap_loc.clone())),
        ast::ExprAST::UnaryOp(opcode, operand, op_loc) => {
            // オペランドの条件式種別を取得
            let operand_kind = _condition_kind(operand)?;

            // Equal or キャプチャ条件式 を返す
            match &operand_kind {
                KindWithRange::Equal(t) => {
                    if let Some(result) = opcode.result_type(*t) {
                        return Ok(KindWithRange::Equal(result));
                    } else {
                        return Err(SemanticError::type_error_unary(
                            opcode,
                            op_loc,
                            *t,
                        ));
                    }
                },
                KindWithRange::Capture(name, cap_loc) => {
                    return Ok(KindWithRange::CaptureCondition((*name).clone(), cap_loc.clone()));
                },
                KindWithRange::CaptureCondition(..) => {
                    return Ok(operand_kind);
                },
            }
        },
        ast::ExprAST::BinaryOp(lhs, opcode, rhs, location) => {
            // 左辺と右辺の条件式種別を取得
            let lhs_kind = _condition_kind(lhs)?;
            let rhs_kind = _condition_kind(rhs)?;

            // 左辺と右辺のどちらかがキャプチャ条件式の場合, キャプチャ条件式を返す
            match (&lhs_kind, &rhs_kind) {
                (
                    KindWithRange::Equal(t_l),
                    KindWithRange::Equal(t_r),
                ) => {
                    if let Some(result) = opcode.result_type(*t_l, *t_r) {
                        return Ok(KindWithRange::Equal(result));
                    } else {
                        return Err(SemanticError::type_error_binary(
                            opcode,
                            location,
                            *t_l,
                            *t_r,
                        ));
                    }
                },

                (
                    KindWithRange::Equal(_),
                    KindWithRange::Capture(name, cap_loc)
                ) | (
                    KindWithRange::Capture(name, cap_loc),
                    KindWithRange::Equal(_)
                ) => {
                    return Ok(KindWithRange::CaptureCondition((*name).clone(), cap_loc.clone()));
                },

                (
                    KindWithRange::Equal(_),
                    KindWithRange::CaptureCondition(..)
                ) => {
                    return Ok(rhs_kind);
                },

                (
                    KindWithRange::CaptureCondition(..),
                    KindWithRange::Equal(_)
                ) => {
                    return Ok(lhs_kind);
                },

                (
                    KindWithRange::Capture(name_l, cap_loc_l),
                    KindWithRange::Capture(name_r, cap_loc_r)
                ) => {
                    if name_l != name_r {
                        return Err(SemanticError::multiple_captures_in_condition(
                            (name_l.clone(), name_r.clone()),
                            (cap_loc_l, cap_loc_r),
                        ));
                    }
                    return Ok(KindWithRange::CaptureCondition((*name_l).clone(), cap_loc_l.clone()));
                },

                (
                    KindWithRange::Capture(name_l, cap_loc_l),
                    KindWithRange::CaptureCondition(name_r, cap_loc_r)
                ) => {
                    if name_l != name_r {
                        return Err(SemanticError::multiple_captures_in_condition(
                            (name_l.clone(), name_r.clone()),
                            (cap_loc_l, cap_loc_r),
                        ));
                    }
                    return Ok(rhs_kind);
                },

                (
                    KindWithRange::CaptureCondition(name_l, cap_loc_l),
                    KindWithRange::Capture(name_r, cap_loc_r)
                ) | (
                    KindWithRange::CaptureCondition(name_l, cap_loc_l),
                    KindWithRange::CaptureCondition(name_r, cap_loc_r)
                ) => {
                    if name_l != name_r {
                        return Err(SemanticError::multiple_captures_in_condition(
                            (name_l.clone(), name_r.clone()),
                            (cap_loc_l, cap_loc_r),
                        ));
                    }
                    return Ok(lhs_kind);
                },
            } // match (&lhs_kind, &rhs_kind)
        },
    } // match expr
}

/// キャプチャ条件式の型推論をする
/// 戻り値: キャプチャの possible_types
fn analyze_capture_condition(
    expr: &ast::ExprAST,
    cond_loc: &Range<usize>,
    capture_name: &String,
    is_typed_capture: bool, // true の場合は条件式の結果が bool 型かのチェックを行わない
) -> Result<HashSet<Type>, SemanticError> {
    // キャプチャに型を 1 つずつ割り当てて検証
    let mut possible_types = HashSet::new();
    let mut cannot_evaluate = true;
    for t in Type::all_types() {
        // type_validate_expr に渡す用の型ヒント
        let captures = {
            let mut captures = HashMap::new();
            captures.insert(capture_name.clone(), t);
            captures
        };
        if let Ok(expr_t) = type_validate_expr(expr, &captures) {
            cannot_evaluate = false;
            if let Type::Bool = expr_t {
                possible_types.insert(t);
            } else if is_typed_capture {
                possible_types.insert(t);
            }
        }
    }

    if possible_types.is_empty() {
        if cannot_evaluate {
            return Err(SemanticError::no_type_matches(cond_loc));
        } else {
            return Err(SemanticError::not_bool_condition(cond_loc));
        }
    }
    Ok(possible_types)
}

// MARK: 出力式の型推論

/// infers 配列のインデックス
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
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
    /// キャプチャを含む単項演算式
    UnaryExpr {
        opcode: UnaryOpcode,
        operand: InfersIndex,
        parent: Option<InfersIndex>,
        result: HashSet<Type>,
        needs_update: bool,
        location: Range<usize>,
    },
    /// キャプチャを含む二項演算式
    BinaryExpr {
        opcode: Opcode,
        lhs: InfersIndex,
        rhs: InfersIndex,
        parent: Option<InfersIndex>,
        result: HashSet<Type>,
        needs_update: bool,
        location: Range<usize>,
    },
}

impl InferredType {
    /// 親ノードを設定
    fn set_parent(&mut self, parent: &InfersIndex) {
        match self {
            InferredType::Constant(_) | InferredType::Capture(_) => (),
            InferredType::UnaryExpr { parent: p, .. } =>
                *p = Some(*parent),
            InferredType::BinaryExpr { parent: p, .. } =>
                *p = Some(*parent),
        }
    }

    /// needs_update を設定
    fn set_needs_update(&mut self, needs_update: bool) {
        match self {
            InferredType::Constant(_) | InferredType::Capture(_) => (),
            InferredType::UnaryExpr { needs_update: n, .. } =>
                *n = needs_update,
            InferredType::BinaryExpr { needs_update: n, .. } =>
                *n = needs_update,
        }
    }

    /// 型を取得
    fn get_types(&self, captures: &HashMap<String, TypeHint>) -> HashSet<Type> {
        match self {
            InferredType::Constant(t) => {
                let mut types = HashSet::new();
                types.insert(*t);
                types
            },
            InferredType::Capture(name) =>
                captures.get(name).unwrap().possible_types.clone(),
            InferredType::UnaryExpr { result, .. } =>
                result.clone(),
            InferredType::BinaryExpr { result, .. } =>
                result.clone(),
        }
    }
}

/// infers 配列のデバッグ用文字列
fn fmt_infers(infers: &Vec<InferredType>, captures: &HashMap<String, TypeHint>) -> String {
    let mut s = String::from("[\n");
    for (i, infer) in infers.iter().enumerate() {
        s.push_str(&format!("{:>4}: ", i));
        if let InferredType::Capture(name) = infer {
            s.push_str(&format!("{:?} {:?}\n", infer, captures[name].possible_types));
        } else {
            s.push_str(&format!("{:?}\n", infer));
        }
    }
    s.push_str("]");
    s
}

/// infers 配列を作成する
fn build_infers(
    outputs: &Vec<ast::OutputAST>,
    captures: &HashMap<String, TypeHint>,
) -> Result<Vec<InferredType>, SemanticError> {
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
    for output in outputs {
        analyze_output_ast(&output.expr, &mut infers, &capture_infers)?;
    }
    Ok(infers)
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
) -> Result<AOAResult, SemanticError> {
    match expr {
        // 定数式の型を返す
        ast::ExprAST::Number(_) => Ok(AOAResult::Constant(Type::Int)),
        ast::ExprAST::Double(_) => Ok(AOAResult::Constant(Type::Double)),
        ast::ExprAST::Str(_) => Ok(AOAResult::Constant(Type::String)),
        ast::ExprAST::Bool(_) => Ok(AOAResult::Constant(Type::Bool)),

        // capture_infers に登録されているインデックスを返す
        ast::ExprAST::Capture(name, cap_loc) => {
            if let Some(index) = capture_infers.get(name) {
                Ok(AOAResult::Infer { index: InfersIndex(*index) })
            } else {
                return Err(SemanticError::undefined_capture(name.clone(), cap_loc));
            }
        },

        // 自身を infers に追加し, そのインデックスを返す
        ast::ExprAST::UnaryOp(opcode, operand, op_loc) => {
            let operand = analyze_output_ast(operand, infers, capture_infers)?;

            // 定数式なら AOAResult::Constant を返す
            if let AOAResult::Constant(t) = &operand {
                if let Some(result) = opcode.result_type(*t) {
                    return Ok(AOAResult::Constant(result));
                } else {
                    return Err(SemanticError::type_error_unary(
                        opcode,
                        op_loc,
                        *t,
                    ));
                }
            }

            // operand の InfersIndex を取得
            let i_operand = match &operand {
                AOAResult::Infer { index } => *index,
                AOAResult::Constant(_) => unreachable!(),
            };

            // 自身を infer に追加する
            infers.push(InferredType::UnaryExpr {
                opcode: *opcode,
                operand: i_operand,
                parent: None,
                result: Type::all_types(),
                needs_update: true,
                location: op_loc.clone(),
            });
            let i_self = InfersIndex(infers.len() - 1);

            // 自身を lhs, rhs の親ノードを設定
            infers[i_operand.0].set_parent(&i_self);

            Ok(AOAResult::Infer { index: i_self })
        }

        // 自身を infers に追加し, そのインデックスを返す
        ast::ExprAST::BinaryOp(lhs, opcode, rhs, op_loc) => {
            let lhs = analyze_output_ast(lhs, infers, capture_infers)?;
            let rhs = analyze_output_ast(rhs, infers, capture_infers)?;

            // 定数式なら AOAResult::Constant を返す
            if let (AOAResult::Constant(t_l), AOAResult::Constant(t_r)) = (&lhs, &rhs) {
                if let Some(result) = opcode.result_type(*t_l, *t_r) {
                    return Ok(AOAResult::Constant(result));
                } else {
                    return Err(SemanticError::type_error_binary(
                        opcode,
                        op_loc,
                        *t_l,
                        *t_r,
                    ));
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
                location: op_loc.clone(),
            });
            let i_self = InfersIndex(infers.len() - 1);

            // 自身を lhs, rhs の親ノードを設定
            infers[i_l.0].set_parent(&i_self);
            infers[i_r.0].set_parent(&i_self);

            Ok(AOAResult::Infer { index: i_self })
        },
    }
}

// infers 配列について型推論を行う
fn infer_infers(
    infers: &mut Vec<InferredType>,
    captures: &mut HashMap<String, TypeHint>
) -> Result<(), SemanticError> {
    let mut updated = true;
    while updated {
        updated = false;
        for myself in (0..infers.len()).map(|i| InfersIndex(i)) {
            // ノードの型を更新する際は, ここに insert する
            let mut updates: HashMap<InfersIndex, HashSet<Type>> = HashMap::new();

            match &infers[myself.0] {
                InferredType::Constant(_) => (),
                InferredType::Capture(_) => (),
                InferredType::UnaryExpr {
                    opcode,
                    operand,
                    result,
                    needs_update,
                    location,
                    ..
                } => {
                    if !*needs_update { continue; }

                    // 推論前の型
                    let t_operand = infers[operand.0].get_types(captures);
                    let t_result = result;

                    // 推論後の型
                    let mut new_t_operand: HashSet<Type> = HashSet::new();
                    let mut new_t_result: HashSet<Type> = HashSet::new();

                    // 推論
                    for signature in UnaryOpcodeSignature::get_signatures(*opcode) {
                        if t_operand.contains(&signature.operand) {
                            new_t_result.insert(signature.result);
                            new_t_operand.insert(signature.operand);
                        }
                    }

                    // 空集合があれば型エラー
                    if new_t_operand.is_empty() || new_t_result.is_empty() {
                        return Err(SemanticError::type_inference_failed_unary(
                            location,
                            &t_operand,
                            opcode,
                            &t_result
                        ));
                    }

                    // 自身と隣接するノードの型を更新
                    if t_operand != new_t_operand {
                        updates.insert(*operand, new_t_operand);
                    }
                    if t_result != &new_t_result {
                        updates.insert(myself, new_t_result);
                    }
                },
                InferredType::BinaryExpr {
                    opcode,
                    lhs,
                    rhs,
                    result,
                    needs_update,
                    location,
                    ..
                } => {
                    if !*needs_update { continue; }

                    // 推論前の型
                    let t_lhs = infers[lhs.0].get_types(captures);
                    let t_rhs = infers[rhs.0].get_types(captures);
                    let t_result = result;

                    // 推論後の型
                    let mut new_t_lhs: HashSet<Type> = HashSet::new();
                    let mut new_t_rhs: HashSet<Type> = HashSet::new();
                    let mut new_t_result: HashSet<Type> = HashSet::new();

                    // 推論
                    for signature in OpcodeSignature::get_signatures(*opcode) {
                        if t_lhs.contains(&signature.lhs) && t_rhs.contains(&signature.rhs)
                            && t_result.contains(&signature.result)
                        {
                            new_t_result.insert(signature.result);
                            new_t_lhs.insert(signature.lhs);
                            new_t_rhs.insert(signature.rhs);
                        }
                    }

                    // 空集合があれば型エラー
                    if new_t_lhs.is_empty() || new_t_rhs.is_empty() || new_t_result.is_empty() {
                        return Err(SemanticError::type_inference_failed_binary(
                            location,
                            &t_lhs, opcode, &t_rhs, &t_result
                        ));
                    }

                    // 自身と隣接するノードの型を更新
                    if t_lhs != new_t_lhs {
                        updates.insert(*lhs, new_t_lhs);
                    }
                    if t_rhs != new_t_rhs {
                        updates.insert(*rhs, new_t_rhs);
                    }
                    if t_result != &new_t_result {
                        updates.insert(myself, new_t_result);
                    }
                },
            } // match infers[myself]

            // ノードの型を更新
            for (index, types) in &updates {
                request_update(index, &types, infers, captures);
            }

            // 更新があった場合のフラグ管理
            if !updates.is_empty() {
                infers[myself.0].set_needs_update(false);
                updated = true;
            }
        } // for myself in index of infers
    } // while updated
    Ok(())
}

/// target の型を types に更新し, 隣接するノードの needs_update を更新にする
fn request_update(
    target: &InfersIndex,
    types: &HashSet<Type>,
    infers: &mut Vec<InferredType>,
    captures: &mut HashMap<String, TypeHint>,
) {
    match &mut infers[target.0] {
        InferredType::Constant(_) => {}
        InferredType::Capture(name) => {
            // captures の型を更新
            captures.get_mut(name).unwrap().possible_types = types.clone();

            // このキャプチャに隣接するノードの needs_update を true にする
            for infer in infers.iter_mut() {
                match infer {
                    InferredType::Constant(_) => (),
                    InferredType::Capture(_) => (),
                    InferredType::UnaryExpr { operand, needs_update, .. } => {
                        if operand == target {
                            *needs_update = true;
                        }
                    },
                    InferredType::BinaryExpr { lhs, rhs, needs_update, .. } => {
                        if lhs == target || rhs == target {
                            *needs_update = true;
                        }
                    },
                }
            }
        },

        InferredType::UnaryExpr { operand, parent, result, .. } => {
            // 自身の型を更新
            *result = types.clone();

            // 隣接するノードの needs_update を true にする
            let (operand, parent) = (operand.0, parent.map(|p| p.0));
            infers[operand].set_needs_update(true);
            if let Some(parent) = parent {
                infers[parent].set_needs_update(true);
            }
        },

        InferredType::BinaryExpr { lhs, rhs, parent, result, .. } => {
            // 自身の型を更新
            *result = types.clone();

            // 隣接するノードの needs_update を true にする
            let (lhs, rhs, parent) = (lhs.0, rhs.0, parent.map(|p| p.0));
            infers[lhs].set_needs_update(true);
            infers[rhs].set_needs_update(true);
            if let Some(parent) = parent {
                infers[parent].set_needs_update(true);
            }
        },
    }
}

/// 出力式の型推論結果の検証
fn validate_inference(
    captures: &HashMap<String, TypeHint>,
    outputs: &Vec<ast::OutputAST>,
) -> Result<(), SemanticError> {
    /// 型検証の失敗情報
    struct FailedCase {
        captures_type: HashMap<String, Type>,
        opcode: Opcode,
        location: Range<usize>,
    }

    // 再帰関数でキャプチャの型の組み合わせを総当たりで検証
    fn _validate_inference(
        captures: &HashMap<String, TypeHint>,
        captures_type: &HashMap<String, Type>,
        output: &ast::OutputAST,
        capture_print_order: &Vec<String>,
    ) -> Result<(), FailedCase> {
        if captures.is_empty() {
            // 型検証
            if let Err(ast) = type_validate_expr(&output.expr, captures_type) {
                match ast {
                    ast::ExprAST::Number(_)|ast::ExprAST::Double(_)|ast::ExprAST::Str(_)|ast::ExprAST::Bool(_)|
                    ast::ExprAST::Capture(_, _) => unreachable!(),
                    // 単項演算子ではキャプチャ間の型制約関係は発生し得ない
                    ast::ExprAST::UnaryOp(..) => unreachable!(),
                    ast::ExprAST::BinaryOp(_, opcode, _, op_loc) =>
                        return Err(FailedCase {
                            captures_type: captures_type.clone(),
                            opcode: opcode.clone(),
                            location: op_loc.clone(),
                        }),
                }
            }
        } else {
            // captures から 1 つ取り出す
            let (name, type_hint) = captures.iter().next().unwrap();

            // 取り出したキャプチャを captures から削除して captures_type に追加
            let mut captures = captures.clone();
            captures.remove(name);
            for t in type_hint.possible_types.iter() {
                let mut captures_type = captures_type.clone();
                captures_type.insert(name.clone(), *t);
                _validate_inference(&captures, &captures_type, output, capture_print_order)?; // 再帰
            }
        }
        Ok(())
    }

    for output in outputs.iter() {
        // captures から output に登場するキャプチャだけを取り出す
        let associated_captures = &output.meta.as_ref().unwrap().associated_captures;
        let captures: HashMap<String, TypeHint> = associated_captures.iter()
            .map(|name| (name.clone(), captures[name].clone()))
            .collect();

        // 再帰関数の呼び出し
        let captures_type = HashMap::new();
        let result = _validate_inference(
            &captures,
            &captures_type,
            output,
            associated_captures
        );

        if let Err(failed_case) = result {
            return Err(SemanticError::type_constraint_detected(
                &captures,
                associated_captures,
                &failed_case.captures_type,
                &failed_case.opcode,
                &failed_case.location,
            ));
        }
    }
    Ok(())
}
