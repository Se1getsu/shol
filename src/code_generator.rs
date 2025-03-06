use core::panic;
use std::collections::HashMap;
use std::io::{ self, Write };
use crate::ast::{ self, Opcode };
use crate::semantics::{ ConditionKind, OpcodeSignatureExt, Type, TypeHint };

// MARK: ヘルパー関数など

impl Type {
    /// 実際の Rust の型名を返す
    fn actual(&self) -> String {
        match self {
            Type::String => "String".to_string(),
            Type::Int => "i32".to_string(),
            Type::Bool => "bool".to_string(),
        }
    }
}

/// 中間コードで使用する識別子を管理する
///
/// 命名規則
/// - `EN_`: 列挙型, 列挙子
/// - `ST_`: 構造体
/// - `TR_`: トレイト
/// - `FN_`: 関数
/// - `ME_`: メンバ変数
/// - `P_`: 仮引数
/// - `V_`: 変数
/// - `_REF`: & 参照
/// - `_MUT`: &mut 参照
struct Identf;
impl Identf {
    /// リソースの型の列挙型
    const EN_TYPE: &'static str = "ResourceType";
    /// コロニーのトレイト
    const TR_COLONY: &'static str = "Colony";
    /// fn (&mut self, P_GIFTS) -> ()
    /// : 送られてきたリソースを受信するメソッド
    const FN_RECEIVE: &'static str = "receive";
    /// fn (&mut self) -> HashMap<usize, Vec<ResourceType>>
    /// : コロニーの規則を実行するメソッド. 戻り値は送信先コロニーのインデックスと送信リソース
    const FN_RULE: &'static str = "rule";
    /// Vec<EN_TYPE>: リソースのメンバ変数
    const ME_RESOURCE: &'static str = "resources";
    /// Vec<EN_TYPE>: 送られてきたリソース
    const P_GIFTS: &'static str = "g";
    /// HashMap<usize, Vec<ResourceType>>: 関数 FN_RULE が戻り値として返す,
    /// 送信先コロニーのインデックスと送信リソースを保持するバッファ
    const V_GIFTS: &'static str = "gifts";
    /// Vec<EN_TYPE>: 規則適用後のリソースを溜めるバッファ
    const V_BUF: &'static str = "buf";
    /// &EN_TYPE: ME_RESOURCE のリソース参照
    const V_RSRS_REF: &'static str = "resource";
    /// bool: どの規則の条件にもマッチしなかったか
    const V_NO_MATCH: &'static str = "no_match";
    /// &Type::actual: リソースの実際の値の参照
    const V_VALUE_REF: &'static str = "v";
    /// HashMap<usize, Vec<ResourceType>>: 新しいリソースと挿入位置をまとめたハッシュマップ
    const V_INSERTIONS: &'static str = "insertions";
    /// Vec<bool>: 1 つ以上の複数条件規則に使われたリソースのフラグ
    const V_SOME_USED: &'static str = "some_used";
    /// Vec<bool>: 現在の複数条件規則で使用済みのリソースのフラグ
    const V_USED: &'static str = "used";
    /// Vec<usize>: 各キャプチャのリソース探索の進度
    const V_CAPT_PROG: &'static str = "capt_prog";
    /// usize: 探索中のキャプチャのインデックス
    const V_CAPT_IDX: &'static str = "capt_idx";
    /// &mut Vec<EN_TYPE>: 出力先エントリへの可変参照
    const V_ENTRY_MUT: &'static str = "entry";
    /// &Vec<EN_TYPE>: V_INSERTIONS の要素への参照
    const V_IELM_REF: &'static str = "insertion";
    /// Vec<Box<dyn TR_COLONY>>: コロニーの構造体のインスタンスを入れるベクタ
    const V_COLONIES: &'static str = "colonies";
    /// usize: for ループで使用
    const V_I: &'static str = "i";

    /// コロニーの構造体
    fn st_colony(name: &str) -> String {
        format!("Colony_{}", name)
    }
    /// EN_TYPE の列挙子
    fn en_type(t: Type) -> String {
        format!("{}::{:?}", Identf::EN_TYPE, t)
    }
    /// &Type::actual: リソースの実際の値の参照
    fn v_value_ref(capture_name: &String) -> String {
        format!("v_{}", capture_name)
    }
}

// MARK: コード生成 - プログラム, コロニー

pub fn generate(
    f: &mut impl Write,
    program: &Vec<ast::StatementAST>,
    src_file: &String,
) -> io::Result<()> {
    // ヘッダ
    writeln!(f, "// Generated by Shol compiler.")?;
    writeln!(f, "// DO NOT EDIT MANUALLY.")?;
    writeln!(f, "//")?;
    writeln!(f, "// Original source: {}", src_file)?;
    writeln!(f, "// Shol version: {}", env!("CARGO_PKG_VERSION"))?;
    writeln!(f, "")?;

    // 警告の非表示
    writeln!(f, "#![allow(")?;
    writeln!(f, "  non_snake_case,")?;
    writeln!(f, "  non_camel_case_types,")?;
    writeln!(f, "  dead_code,")?;
    writeln!(f, "  unused_variables,")?;
    writeln!(f, "  unused_imports,")?;
    writeln!(f, "  unused_mut,")?;
    writeln!(f, "  unreachable_patterns,")?;
    writeln!(f, ")]")?;

    // use 宣言
    writeln!(f, "use std::collections::HashMap;")?;
    writeln!(f, "")?;

    // 型定義
    writeln!(f, "#[derive(Eq,Debug,PartialEq,Clone)]")?;
    writeln!(f, "enum {} {{", Identf::EN_TYPE)?;
    for t in Type::all_types() {
        writeln!(f, "  {:?}({}),", t, t.actual())?;
    }
    writeln!(f, "}}")?;

    // コロニートレイト定義
    writeln!(f, "trait Colony {{")?;
    writeln!(f, "  fn {}(&mut self, gifts: Vec<{}>);", Identf::FN_RECEIVE, Identf::P_GIFTS)?;
    writeln!(f, "  fn {}(&mut self) -> HashMap<usize, Vec<{}>>;", Identf::FN_RULE, Identf::EN_TYPE)?;
    writeln!(f, "}}")?;

    // コロニー名と V_COLONIES のインデックス対応表作成
    let colony_indices: HashMap<&String, usize> = {
        let mut colony_indices = HashMap::new();
        let mut count = 0;
        for stmt in program {
            match stmt {
                ast::StatementAST::ColonyDecl { name, .. } |
                ast::StatementAST::ColonyExtension { name, .. } => {
                    colony_indices.insert(name, count);
                    count += 1;
                }
            }
        }
        colony_indices
    };

    // 各コロニーを定義
    for stmt in program {
        writeln!(f, "")?;
        match stmt {
            ast::StatementAST::ColonyDecl { name, rules, .. } =>
                generate_colony_decl(f, name, rules, &colony_indices)?,
            ast::StatementAST::ColonyExtension { name, rules, .. } =>
                generate_colony_extension(f, name, rules, &colony_indices)?,
        }
    }

    // メイン関数
    writeln!(f, "")?;
    writeln!(f, "fn main() {{")?;

    // V_COLONIES の作成
    writeln!(f, "  let mut {}: Vec<Box<dyn {}>> = Vec::new();", Identf::V_COLONIES, Identf::TR_COLONY)?;
    for stmt in program {
        match stmt {
            ast::StatementAST::ColonyDecl { name, resources, .. } |
            ast::StatementAST::ColonyExtension { name, resources, .. } => {
                writeln!(f, "  {}.push(Box::new({} {{", Identf::V_COLONIES, Identf::st_colony(name))?;
                writeln!(f, "    {}: vec![", Identf::ME_RESOURCE)?;
                for resource in resources {
                    write!(f, "      ")?;
                    generate_resource(f, resource, &|_, name| {
                        // リソースは文法上リテラルしか許容してないので, このクロージャは呼ばれないはず
                        panic!("リソースにキャプチャが含まれます: ${}", name)
                    })?;
                    writeln!(f, ",")?;
                }
                writeln!(f, "    ],")?;
                writeln!(f, "  }}));")?;
            },
        }
    }

    // TODO: 規則の呼び出し

    writeln!(f, "}}")?; // fn main

    Ok(())
}

fn generate_colony_decl(
    f: &mut impl Write,
    name: &str,
    rules: &Vec<ast::RuleSetAST>,
    colony_indices: &HashMap<&String, usize>,
) -> io::Result<()> {
    let colony_name = Identf::st_colony(name);

    writeln!(f, "struct {} {{", colony_name)?;
    writeln!(f, "  {}: HashMap<String, {}>,", Identf::ME_RESOURCE, Identf::EN_TYPE)?;
    writeln!(f, "}}")?;
    writeln!(f, "impl {} for {} {{", Identf::TR_COLONY, colony_name)?;
    writeln!(f, "  fn {}(&mut self, {}: Vec<{}>) {{ self.{}.extend({}); }}",
        Identf::FN_RECEIVE, Identf::P_GIFTS, Identf::EN_TYPE, Identf::ME_RESOURCE, Identf::P_GIFTS)?;
    writeln!(f, "  fn {}(&mut self) {{", Identf::FN_RULE)?;
    writeln!(f, "    let mut {}: HashMap<usize, Vec<{}>> = HashMap::new();",
        Identf::V_GIFTS, Identf::EN_TYPE)?;
    for rule_set in rules {
        generate_rule_set(f, rule_set, colony_indices)?;
    }
    writeln!(f, "    {}", Identf::V_GIFTS)?;
    writeln!(f, "  }}")?;
    writeln!(f, "}}")?;

    Ok(())
}

fn generate_colony_extension(
    f: &mut impl Write,
    name: &str,
    _rules: &Vec<ast::RuleSetAST>,
    _colony_indices: &HashMap<&String, usize>,
) -> io::Result<()> {
    let colony_name = Identf::st_colony(name);

    writeln!(f, "struct {} {{", colony_name)?;
    writeln!(f, "  {}: HashMap<String, {}>,", Identf::ME_RESOURCE, Identf::EN_TYPE)?;
    writeln!(f, "}}")?;
    writeln!(f, "impl {} for {} {{", Identf::TR_COLONY, colony_name)?;
    writeln!(f, "  fn {}(&mut self, {}: Vec<{}>) {{ self.{}.extend({}); }}",
        Identf::FN_RECEIVE, Identf::P_GIFTS, Identf::EN_TYPE, Identf::ME_RESOURCE, Identf::P_GIFTS)?;
    writeln!(f, "  fn {}(&mut self) {{", Identf::FN_RULE)?;
    writeln!(f, "    let mut {} = HashMap::new();", Identf::V_GIFTS)?;
    writeln!(f, "    todo!();")?;
    writeln!(f, "    {}", Identf::V_GIFTS)?;
    writeln!(f, "  }}")?;
    writeln!(f, "}}")?;

    Ok(())
}

// MARK: コード生成 - 規則

fn generate_rule_set(
    f: &mut impl Write,
    rule_set: &ast::RuleSetAST,
    colony_indices: &HashMap<&String, usize>,
) -> io::Result<()> {
    // 単一条件規則が 1 つ以上存在するか
    let has_single_cond_rule = rule_set.rules
        .iter().any(|rule| rule.conditions.len() == 1);
    // 複数条件規則が 1 つ以上存在するか
    let has_multi_cond_rule = rule_set.rules
        .iter().any(|rule| rule.conditions.len() >= 2);

    if has_multi_cond_rule {
        // 前処理: 複数条件規則の結果を格納する変数
        writeln!(f, "    let mut {}: HashMap<usize, Vec<ResourceType>> = HashMap::new();",
            Identf::V_INSERTIONS)?;
        writeln!(f, "    let mut {}: Vec<bool> = vec![false; self.{}.len()];",
            Identf::V_SOME_USED, Identf::ME_RESOURCE)?;

        // 前処理: 複数条件規則を適用して, 上記の変数に結果を格納
        for rule in &rule_set.rules {
            if !(rule.conditions.len() >= 2) { continue; }
            generate_multi_condition_rule(f, rule, colony_indices)?;
        }
    }

    // メイン: 各リソースを for 文で処理
    writeln!(f, "    let mut {} = Vec::new();", Identf::V_BUF)?;
    writeln!(f, "    for {} in self.{}.iter() {{", Identf::V_RSRS_REF, Identf::ME_RESOURCE)?;
    writeln!(f, "      let mut {} = true;", Identf::V_NO_MATCH)?;

    // メイン: 複数条件規則の結果を適用
    if has_multi_cond_rule {
        writeln!(f, "      {} &= !{}[{}];", Identf::V_NO_MATCH, Identf::V_SOME_USED, Identf::V_I)?;
        writeln!(f, "      if let Some({}) = {}.get(&{}) {{",
            Identf::V_IELM_REF, Identf::V_INSERTIONS, Identf::V_I)?;
        writeln!(f, "        {}.extend({}.clone());", Identf::V_BUF, Identf::V_INSERTIONS)?;
        writeln!(f, "      }}",)?;
    }

    // メイン: 単一条件規則の規則を適用
    if has_single_cond_rule {
        writeln!(f, "      match {} {{", Identf::V_RSRS_REF)?;
        for t in Type::all_types() {
            writeln!(f, "        {}({}) => {{", Identf::en_type(t), Identf::V_VALUE_REF)?;
            for rule in &rule_set.rules {
                // 単一条件か確認
                if !(rule.conditions.len() == 1) { continue; }
                // t が条件式のキャプチャ型と一致するか確認
                let first_capture = rule.meta.as_ref()
                    .unwrap().captures.iter().next();
                if let Some((_, typehint)) = first_capture {
                    if !(typehint.possible_types.contains(&t)) { continue; }
                }
                // 規則を適用するコードを生成
                generate_single_condition_rule(f, rule, t)?;
            }
            writeln!(f, "        }}")?;
        }
        writeln!(f, "      }}")?;
    }

    // メイン: どの規則にもマッチしなかったリソースはそのままバッファに追加
    writeln!(f, "      if {} {{", Identf::V_NO_MATCH)?;
    writeln!(f, "        {}.push({}.clone());", Identf::V_BUF, Identf::V_RSRS_REF)?;
    writeln!(f, "      }}")?;

    writeln!(f, "    }}")?;
    writeln!(f, "    self.{} = {};", Identf::ME_RESOURCE, Identf::V_BUF)?;
    Ok(())
}

// MARK: コード生成 - 規則 - 前処理

/// ME_RESOURCE の for 文の前に, 複数条件規則を適用するコードを生成
/// NOTE: rule が複数条件式であることを確認してください
fn generate_multi_condition_rule(
    f: &mut impl Write,
    rule: &ast::RuleAST,
    colony_indices: &HashMap<&String, usize>,
) -> io::Result<()> {
    let captures = &rule.meta.as_ref().unwrap().captures;

    writeln!(f, "    let mut {}: Vec<bool> = vec![false; self.{}.len()];",
        Identf::V_USED, Identf::ME_RESOURCE)?;
    writeln!(f, "    let mut {}: Vec<usize> = vec![0;{}];",
        Identf::V_CAPT_PROG, rule.conditions.len())?;
    writeln!(f, "    let mut {}: usize = 0;", Identf::V_CAPT_IDX)?;

    // 条件に当てはまるリソースがこれ以上見つからなかった時点で終了
    writeln!(f, "    while {}[{}] < self.{}.len() {{",
        Identf::V_CAPT_PROG, Identf::V_CAPT_IDX, Identf::ME_RESOURCE)?;

    // 使用済みのリソースはスキップ
    writeln!(f, "      if {}[{}[{}]] {{",
        Identf::V_USED, Identf::V_CAPT_PROG, Identf::V_CAPT_IDX)?;
    writeln!(f, "        {}[{}] += 1;", Identf::V_CAPT_PROG, Identf::V_CAPT_IDX)?;
    writeln!(f, "        continue;")?;
    writeln!(f, "      }}")?;

    writeln!(f, "      match {} {{", Identf::V_CAPT_IDX)?;
    for (i, condition) in rule.conditions.iter().enumerate() {
        let is_last = i == rule.conditions.len() - 1;
        let next_idx = if is_last { 0 } else { i + 1 };
        writeln!(f, "        {} => {{", i)?;

        // ↓ generates:        if condition {
        generate_multi_condition_judge(f, condition, i, captures)?;
        writeln!(f, "            {} = {};", Identf::V_CAPT_IDX, next_idx)?;
        writeln!(f, "            {}[{}[{}]] = true;", Identf::V_USED, Identf::V_CAPT_PROG, i)?;
        writeln!(f, "            {}[{}[{}]] = true;", Identf::V_SOME_USED, Identf::V_CAPT_PROG, i)?;
        if is_last {
            // 出力リソースを出力先に push
            let capts_ref_code =
                multi_condition_capts_ref_code(&rule.conditions);
            generate_multi_condition_outputs(
                f,
                &rule.outputs,
                captures,
                &capts_ref_code,
                colony_indices,
            )?;
        }
        writeln!(f, "          }}")?; // if condition

        writeln!(f, "          {}[{}] += 1;", Identf::V_CAPT_PROG, i)?;
        writeln!(f, "        }},")?;
    }
    writeln!(f, "        _ => unreachable!()")?;
    writeln!(f, "      }}")?;

    writeln!(f, "    }}")?; // end while

    // 使用済みフラグを立てたが, 最後の条件式までリソースが揃わなかったものを解除
    writeln!(f, "    for {} in 0..{} {{", Identf::V_I, Identf::V_CAPT_IDX)?;
    writeln!(f, "      {}[{}[{}]-1] = false;", Identf::V_USED, Identf::V_CAPT_PROG, Identf::V_I)?;
    writeln!(f, "      {}[{}[{}]-1] = false;", Identf::V_SOME_USED, Identf::V_CAPT_PROG, Identf::V_I)?;
    writeln!(f, "    }}")?;

    Ok(())
}

/// 複数条件規則の前処理の `if condition {` 部分を生成
fn generate_multi_condition_judge(
    f: &mut impl Write,
    condition: &ast::ConditionAST,
    cond_idx: usize,
    captures: &HashMap<String, TypeHint>,
) -> io::Result<()> {
    writeln!(f, "          if match &self.{}[{}[{}]] {{",
            Identf::ME_RESOURCE, Identf::V_CAPT_PROG, cond_idx)?;

    match &condition.meta.as_ref().unwrap().kind {
        ConditionKind::Equal(_) => {
            let (result_type, expr_str) = {
                let mut buffer = Vec::new();
                let result_type = generate_expr(&mut buffer, &condition.expr, &|_, _| {
                    unreachable!()
                })?;
                (result_type, String::from_utf8(buffer).unwrap())
            };
            writeln!(f, "            {}({}) => *{} == {},", Identf::en_type(result_type),
                Identf::V_VALUE_REF, Identf::V_VALUE_REF, expr_str)?;
        },
        ConditionKind::Capture(name) => {
            let types_str = captures[name].possible_types
                .iter().map(|t| {
                    format!("{}(_)", Identf::en_type(*t))
                }).collect::<Vec<String>>()
                .join("|");
            writeln!(f, "            {} => true,", types_str)?;
        },
        ConditionKind::CaptureCondition(name) => {
            for t in &captures[name].possible_types {
                write!(f, "            {}({}) => ", Identf::en_type(*t), Identf::V_VALUE_REF)?;
                generate_expr(f, &condition.expr, &|f, _| {
                    write!(f, "*{}", Identf::V_VALUE_REF)?;
                    Ok(*t)
                })?;
                writeln!(f, ",")?;
            }
        }
    }

    writeln!(f, "            _ => false")?;
    writeln!(f, "          }} {{")?;
    Ok(())
}

/// キャプチャの名前と, そのキャプチャを参照するコードの対応表を作る
fn multi_condition_capts_ref_code(
    conditions: &Vec<ast::ConditionAST>,
) -> HashMap<String, String> {
    let mut capts_ref_code = HashMap::new();
    for (cond_idx, condition) in conditions.iter().enumerate() {
        let cond_meta = condition.meta.as_ref().unwrap();
        match &cond_meta.kind {
            ConditionKind::Equal(_) => (),
            ConditionKind::Capture(name) |
            ConditionKind::CaptureCondition(name) => {
                capts_ref_code.insert(
                    name.clone(),
                    format!("&self.{}[{}[{}]-1]", Identf::ME_RESOURCE,
                        Identf::V_CAPT_PROG, cond_idx)
                );
            },
        }
    }
    capts_ref_code
}

/// 複数条件規則の前処理の, 出力リソースを出力先に push する部分を生成
fn generate_multi_condition_outputs(
    f: &mut impl Write,
    outputs: &Vec<ast::OutputAST>,
    captures: &HashMap<String, TypeHint>,
    capts_ref_code: &HashMap<String, String>,
    colony_indices: &HashMap<&String, usize>,
) -> io::Result<()> {
    let mut prev_cindex: Option<Option<usize>> = None;
    for output in outputs {
        // 出力先コロニーのインデックス
        let cindex: Option<usize> = output.destination.as_ref().map(|dest| {
            let index = colony_indices.get(dest)
                .expect(&format!("未定義のコロニーへの出力: {dest}"));
            *index
        });
        // 出力先エントリの取り出し (直前に同じエントリを取り出したならスキップ)
        if prev_cindex != Some(cindex) {
            if let Some(index) = cindex {
                writeln!(f, "            let {} = {}.entry({}).or_default();",
                    Identf::V_ENTRY_MUT, Identf::V_GIFTS, index)?;
            } else {
                writeln!(f, "            let {} = {}.entry({}[0]-1).or_default();",
                    Identf::V_ENTRY_MUT, Identf::V_INSERTIONS, Identf::V_CAPT_PROG)?;
            }
        }
        // 出力コード生成
        generate_multi_condition_output(f, output, captures, &capts_ref_code)?;

        prev_cindex = Some(cindex);
    }
    Ok(())
}

/// generate_multi_condition_outputs での個別 output の生成処理
fn generate_multi_condition_output(
    f: &mut impl Write,
    output: &ast::OutputAST,
    captures: &HashMap<String, TypeHint>,
    capts_ref_code: &HashMap<String, String>,
) -> io::Result<()> {
    /// 出力リソース push 部分のコードを生成
    fn _generate_push_resource(
        f: &mut impl Write,
        output_expr: &ast::ExprAST,
        generate_capture: &impl Fn(&mut dyn Write, &String) -> io::Result<Type>
    ) -> io::Result<()> {
        let (result_type, expr_str) = {
            let mut buffer = Vec::new();
            let result_type = generate_expr(&mut buffer, output_expr, generate_capture)?;
            (result_type, String::from_utf8(buffer).unwrap())
        };
        write!(f, "{}.push({}({}))", Identf::V_ENTRY_MUT, Identf::en_type(result_type), expr_str)?;
        Ok(())
    }

    /// 再帰関数で assoc_capts の型を網羅的に決定
    fn backtrack_types(
        f: &mut impl Write,
        assoc_captures: &HashMap<String, TypeHint>,
        types: &HashMap<String, Type>,
        output_expr: &ast::ExprAST,
        assoc_capts: &Vec<String>,
    ) -> io::Result<()> {
        if assoc_captures.is_empty() {
            write!(f, "              (")?;
            for capt in assoc_capts {
                write!(f, "{}({}),", Identf::en_type(types[capt]), Identf::v_value_ref(capt))?;
            }
            writeln!(f, ") =>")?;
            write!(f, "                ")?;
            _generate_push_resource(f, output_expr, &|f, name| {
                write!(f, "{}", Identf::v_value_ref(name))?;
                Ok(types[name])
            })?;
            writeln!(f, ",")?;

        } else {
            // assoc_captures から 1 つ取り出す
            let (name, type_hint) = assoc_captures.iter().next().unwrap();

            // 取り出したキャプチャを assoc_captures から削除して types に追加
            let mut assoc_captures = assoc_captures.clone();
            assoc_captures.remove(name);
            for t in type_hint.possible_types.iter() {
                let mut types = types.clone();
                types.insert(name.clone(), *t);
                backtrack_types(f, &assoc_captures, &types, output_expr, assoc_capts)?; // 再帰
            }
        }
        Ok(())
    }

    // 関連キャプチャ: 出力式に含まれるキャプチャ
    let assoc_capts = &output.meta.as_ref().unwrap().associated_captures;

    // 関連キャプチャがなければそのまま push
    if assoc_capts.is_empty() {
        write!(f, "            ")?;
        _generate_push_resource(f, &output.expr, &|_, _| {
            unreachable!()
        })?;
        writeln!(f, ";")?;
        return Ok(());
    }

    // 関連キャプチャの型の組み合わせごとに push のコードを生成する
    write!(f, "            match (")?;
    for capt in assoc_capts {
        write!(f, "{},", capts_ref_code[capt])?;
    }
    writeln!(f, ") {{")?;

    let mut assoc_captures = HashMap::new();
    for capt in assoc_capts {
        assoc_captures.insert(capt.clone(), captures[capt].clone());
    }
    let types = HashMap::new();
    backtrack_types(f, &assoc_captures, &types, &output.expr, assoc_capts)?;

    writeln!(f, "              _ => ()")?;
    writeln!(f, "            }}")?;
    Ok(())
}

// MARK: コード生成 - 規則 - メインの for 文内の処理

/// ME_RESOURCE の for 文内で, 単一条件規則を適用するコードを生成
/// NOTE: rule が単一条件規則であることを確認してください
fn generate_single_condition_rule(
    f: &mut impl Write,
    rule: &ast::RuleAST,
    capture_type: Type,
) -> io::Result<()> {
    let cond = &rule.conditions[0];
    let cond_meta = cond.meta.as_ref().unwrap();
    write!(f, "          ")?;
    match cond_meta.kind {
        // 条件式と一致したら出力式を push
        ConditionKind::Equal(_) => {
            write!(f, "if *{} == ", Identf::V_VALUE_REF)?;
            generate_expr(f, &cond.expr, &|_, _| {
                unreachable!()
            })?;
            write!(f, " ")?;
        },
        // 無条件で出力式を push
        ConditionKind::Capture(_) => (),
        // 条件式を満たすならば出力式を push
        ConditionKind::CaptureCondition(_) => {
            write!(f, "if ")?;
            generate_expr(f, &cond.expr, &|f, _| {
                write!(f, "*{}", Identf::V_VALUE_REF)?;
                Ok(capture_type)
            })?;
            write!(f, " ")?;
        },
    }
    writeln!(f, "{{")?;
    for output in &rule.outputs {
        write!(f, "            {}.push(", Identf::V_BUF)?;
        generate_resource(f, &output.expr, &|f, _| {
            write!(f, "*{}", Identf::V_VALUE_REF)?;
            Ok(capture_type)
        })?;
        writeln!(f, ");")?;
    }
    writeln!(f, "            {} = false;", Identf::V_NO_MATCH)?;
    writeln!(f, "          }}")?;
    Ok(())
}

// MARK: コード生成 - 式

fn generate_resource(
    f: &mut impl Write,
    expr: &ast::ExprAST,
    generate_capture: &impl Fn(&mut dyn Write, &String) -> io::Result<Type>,
) -> io::Result<()> {
    let (result_type, expr_str) = {
        let mut buffer = Vec::new();
        let result_type = generate_expr(&mut buffer, expr, generate_capture)?;
        (result_type, String::from_utf8(buffer).unwrap())
    };
    write!(f, "{}({})", Identf::en_type(result_type), expr_str)?;
    Ok(())
}

fn generate_expr(
    f: &mut impl Write,
    expr: &ast::ExprAST,
    generate_capture: &impl Fn(&mut dyn Write, &String) -> io::Result<Type>,
) -> io::Result<Type> {
    let result_type: Type;
    match expr {
        ast::ExprAST::Number(i) => {
            write!(f, "{}", i)?;
            result_type = Type::Int;
        },
        ast::ExprAST::Str(s) => {
            write!(f, "{:?}", s)?;
            result_type = Type::String;
        },
        ast::ExprAST::Capture(name) => {
            return generate_capture(f, name);
        },
        ast::ExprAST::BinaryOp(lhs, opcode, rhs) => {
            write!(f, "(",)?;
            let lhs_type = generate_expr(f, lhs, generate_capture)?;
            match opcode {
                Opcode::Mul => write!(f, "*",)?,
                Opcode::Div => write!(f, "/",)?,
                Opcode::Mod => write!(f, "%",)?,
                Opcode::Add => write!(f, "+",)?,
                Opcode::Sub => write!(f, "-",)?,
                Opcode::Eq => write!(f, "==",)?,
                Opcode::Ne => write!(f, "!=",)?,
                Opcode::Lt => write!(f, "<",)?,
                Opcode::Gt => write!(f, ">",)?,
                Opcode::Le => write!(f, "<=",)?,
                Opcode::Ge => write!(f, ">=",)?,
            }
            let rhs_type = generate_expr(f, rhs, generate_capture)?;
            write!(f, ")",)?;

            match opcode.result_type(lhs_type, rhs_type) {
                Some(t) => result_type = t,
                None => panic!("不正な型の演算です: {:?} {:?} {:?}", lhs_type, opcode, rhs_type),
            }
        }
    }
    Ok(result_type)
}
