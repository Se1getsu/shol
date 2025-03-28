use core::panic;
use std::collections::HashMap;
use std::io::{ self, Write };
use crate::ast::{ self, UnaryOpcode, Opcode };
use crate::semantics::{ self, BuiltinColony, ConditionKind, UnaryOpcodeSignatureExt, OpcodeSignatureExt, Type, TypeHint };

// MARK: ヘルパー関数など

impl Type {
    /// 実際の Rust の型名を返す
    fn actual(&self) -> String {
        match self {
            Type::String => "String".to_string(),
            Type::Int => "i32".to_string(),
            Type::Double => "f64".to_string(),
            Type::Bool => "bool".to_string(),
            Type::Symbol => "usize".to_string(),
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
    /// fn (&mut self) -> Result<
    ///     HashMap<usize, Vec<EN_TYPE>>,
    ///     ExitCode
    /// >
    /// : コロニーの規則を実行するメソッド. 戻り値は送信先コロニーのインデックスと送信リソース
    const FN_RULE: &'static str = "rule";
    /// fn (i32) -> String
    /// : 整数を文字列に変換するユーティリティ関数
    const FN_UTIL_CHR: &'static str = "chr";
    /// fn (String) -> i32
    /// : 文字列の最初の文字を整数に変換するユーティリティ関数
    const FN_UTIL_ORD: &'static str = "ord";
    /// fn (&str, Option<i32>, Option<i32>, Option<i32>) -> String
    /// : 文字列のスライスを行うユーティリティ関数
    const FN_UTIL_SLICE: &'static str = "slice";
    /// fn (String, i32) -> String
    /// : 文字列の n 番目の文字を取得するユーティリティ関数
    const FN_UTIL_NTH: &'static str = "nth";

    /// Vec<EN_TYPE>: リソースのメンバ変数
    const ME_RESOURCE: &'static str = "resources";

    /// Vec<EN_TYPE>: 送られてきたリソース
    const P_GIFTS: &'static str = "g";
    /// &Type::actual: ユーティリティ関数の引数
    const P_UTIL_ARG: &'static str = "a";
    /// &str: FN_UTIL_SLICE の引数
    const P_SLICE_S: &'static str = "s";
    /// Option<i32>: FN_UTIL_SLICE の引数
    const P_SLICE_START: &'static str = "start";
    /// Option<i32>: FN_UTIL_SLICE の引数
    const P_SLICE_END: &'static str = "end";
    /// Option<i32>: FN_UTIL_SLICE の引数
    const P_SLICE_STEP: &'static str = "step";
    /// String: FN_UTIL_NTH の引数
    const P_NTH_S: &'static str = "s";
    /// i32: FN_UTIL_NTH の引数
    const P_NTH_N: &'static str = "n";

    /// [&str;n]: シンボルの名前対応表
    const V_SYMBOLS: &'static str = "SYMBOLS";
    /// HashMap<usize, Vec<ResourceType>>: 関数 FN_RULE が戻り値として返す,
    /// 送信先コロニーのインデックスと送信リソースを保持するバッファ
    const V_GIFTS: &'static str = "gifts";
    /// usize: V_GIFTS のキー (送信先コロニーのインデックス)
    const V_GIFTS_DEST: &'static str = "d";
    /// Vec<EN_TYPE>: V_GIFTS の要素 (送信リソース)
    const V_GIFTS_VEC: &'static str = "gv";
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
    /// ExitCode: プログラムの終了コード
    const V_CODE: &'static str = "c";
    /// mpsc::Sender<String>: 標準入力を受け取るチャンネルの送信者
    const V_TX: &'static str = "tx";
    /// mpsc::Receiver<String>: 標準入力を受け取るチャンネルの受信者
    const V_RX: &'static str = "rx";
    /// Result<String, Error>, String: 受信した標準入力
    const V_LINE: &'static str = "l";
    /// &mut fmt::Formatter<'_>: fmt::Debug, fmt::Display の impl で使用
    const V_F_MUT: &'static str = "f";
    /// i32: FN_UTIL_SLICE のローカル変数
    const V_SLICE_LEN: &'static str = "len";
    /// String: FN_UTIL_SLICE のローカル変数
    const V_SLICE_R: &'static str = "r";

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
    program: &ast::ProgramAST,
    src_file: &String,
) -> io::Result<()> {
    // ProgramAST のメタデータを取得
    let program_meta = program.meta.as_ref().unwrap();

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
    writeln!(f, "  unused_parens,")?;
    writeln!(f, "  unused_assignments,")?;
    writeln!(f, ")]")?;

    // use 宣言
    writeln!(f, "use std::{{\
        collections::HashMap,\
        fmt,\
        io::{{self, BufRead, Write}},\
        process::ExitCode,\
        sync::mpsc,\
        thread,\
        time::Duration\
    }};")?;
    writeln!(f, "")?;

    // シンボルの名前対応表を定義
    let symbol_names: HashMap<_, _> = program_meta.symbol_values
        .iter()
        .map(|(k, v)| (*v, k))
        .collect();
    write!(f, "static {}: [&str; {}] = [", Identf::V_SYMBOLS, symbol_names.len())?;
    for i in 0..symbol_names.len() {
        write!(f, "\"{}\",", symbol_names[&i])?;
    }
    writeln!(f, "];")?;

    // 型定義
    writeln!(f, "#[derive(PartialEq,Clone)]")?;
    writeln!(f, "enum {} {{", Identf::EN_TYPE)?;
    for t in Type::all_types() {
        writeln!(f, "  {:?}({}),", t, t.actual())?;
    }
    writeln!(f, "}}")?;

    writeln!(f, "impl fmt::Debug for {} {{", Identf::EN_TYPE)?;
    writeln!(f, "  fn fmt(&self, {}: &mut fmt::Formatter<'_>) -> fmt::Result {{", Identf::V_F_MUT)?;
    writeln!(f, "    match self {{")?;
    writeln!(f, "      {0}({1}) => write!({2}, \"'{{}}\", {3}[*{1}]),",
        Identf::en_type(Type::Symbol), Identf::V_VALUE_REF, Identf::V_F_MUT, Identf::V_SYMBOLS)?;
    for t in vec![
        Type::Bool,
        Type::Double,
        Type::Int,
        Type::String,
    ] {
        writeln!(f, "      {0}({1}) => write!({2}, \"{{{1}:?}}\"),",
            Identf::en_type(t), Identf::V_VALUE_REF, Identf::V_F_MUT)?;
    }
    writeln!(f, "    }}")?;
    writeln!(f, "  }}")?;
    writeln!(f, "}}")?;

    writeln!(f, "impl fmt::Display for {} {{", Identf::EN_TYPE)?;
    writeln!(f, "  fn fmt(&self, {}: &mut fmt::Formatter<'_>) -> fmt::Result {{", Identf::V_F_MUT)?;
    writeln!(f, "    match self {{")?;
    writeln!(f, "      {0}({1}) => write!({2}, \"{{}}\", {3}[*{1}]),",
        Identf::en_type(Type::Symbol), Identf::V_VALUE_REF, Identf::V_F_MUT, Identf::V_SYMBOLS)?;
    for t in vec![
        Type::Bool,
        Type::Double,
        Type::Int,
        Type::String,
    ] {
        writeln!(f, "      {0}({1}) => write!({2}, \"{{{1}}}\"),",
            Identf::en_type(t), Identf::V_VALUE_REF, Identf::V_F_MUT)?;
    }
    writeln!(f, "    }}")?;
    writeln!(f, "  }}")?;
    writeln!(f, "}}")?;

    // ユーティリティ関数
    writeln!(f, "fn {0}({1}:i32)->String{{\
        char::from_u32({1} as u32).map(String::from).unwrap_or_default()\
    }}", Identf::FN_UTIL_CHR, Identf::P_UTIL_ARG)?;

    writeln!(f, "fn {0}({1}:String)->i32{{\
        {1}.chars().next().unwrap_or(\'\\0\')as i32\
    }}", Identf::FN_UTIL_ORD, Identf::P_UTIL_ARG)?;

    writeln!(f, "fn {nth}({s}:String,{n}:i32)->String{{\
        {s}.chars().nth({n} as usize).map_or_else(String::new,String::from)\
    }}", nth=Identf::FN_UTIL_NTH, s=Identf::P_NTH_S, n=Identf::P_NTH_N)?;

    writeln!(f, "fn {slice}({s}:&str,{start}:Option<i32>,{end}:Option<i32>,{step}:Option<i32>)->String{{
  let {s}: Vec<char> = {s}.chars().collect();
  let {len} = {s}.len() as i32;

  let {step} = {step}.unwrap_or(1);
  if {step} == 0 {{ return String::new(); }}

  let {start} = match {start} {{
    Some({start})=>if {start}<0{{ ({start}+{len}).max(0) }}else{{ {start}.clamp(0,{len}-1) }},
    None=>if {step}>0{{ 0 }}else{{ {len}-1 }}
  }};
  let {end} = match {end} {{
    Some({end})=>if {end}<0{{ ({end}+{len}).max(0) }}else{{ {end}.clamp(0,{len}) }},
    None=>if {step}>0{{ {len} }}else{{ -1 }}
  }};

  let mut {r} = String::new();
  if {step} > 0 {{
    let mut {i} = {start};
    while {i} < {end} {{
      {r}.push({s}[{i} as usize]);
      {i} += {step};
    }}
  }} else {{
    let mut {i} = {start};
    while {i} > {end} {{
      {r}.push({s}[{i} as usize]);
      {i} += {step};
    }}
  }}
  {r}
}}", slice=Identf::FN_UTIL_SLICE, s=Identf::P_SLICE_S, start=Identf::P_SLICE_START, end=Identf::P_SLICE_END, step=Identf::P_SLICE_STEP,
    len=Identf::V_SLICE_LEN, r=Identf::V_SLICE_R, i=Identf::V_I)?;

    // コロニートレイト定義
    writeln!(f, "trait {} {{", Identf::TR_COLONY)?;
    writeln!(f, "  fn {}(&mut self, {}: Vec<{}>);", Identf::FN_RECEIVE, Identf::P_GIFTS, Identf::EN_TYPE)?;
    writeln!(f, "  fn {}(&mut self) -> Result<HashMap<usize, Vec<{}>>, ExitCode>;",
        Identf::FN_RULE, Identf::EN_TYPE)?;
    writeln!(f, "}}")?;

    // 各コロニーを定義
    for stmt in &program.statements {
        writeln!(f, "")?;
        match stmt {
            ast::StatementAST::ColonyDecl { name, rules, .. } => {
                generate_colony_decl(f, name, rules, program_meta)?;
            },
            ast::StatementAST::ColonyExtension { name, rules, meta, .. } => {
                generate_colony_extension(f, name, rules, meta, program_meta)?;
            },
        }
    }

    // メイン関数
    writeln!(f, "")?;
    writeln!(f, "fn main() -> ExitCode{{")?;

    // V_COLONIES の作成
    writeln!(f, "  let mut {}: Vec<Box<dyn {}>> = Vec::new();", Identf::V_COLONIES, Identf::TR_COLONY)?;
    for stmt in &program.statements {
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
                    }, program_meta)?;
                    writeln!(f, ",")?;
                }
                writeln!(f, "    ],")?;
                writeln!(f, "  }}));")?;
            },
        }
    }

    // 標準入力を受け付けるスレッドを作成
    let contains_cin = program_meta.colony_indices.contains_key(&"cin".to_string());
    if contains_cin {
        writeln!(f, "  let ({}, {}) = mpsc::channel();", Identf::V_TX, Identf::V_RX)?;
        writeln!(f, "  thread::spawn(move || {{")?;
        writeln!(f, "    for {} in io::stdin().lock().lines() {{", Identf::V_LINE)?;
        writeln!(f, "      if let Ok({0}) = {0} {{", Identf::V_LINE)?;
        writeln!(f, "        if tx.send({}).is_err() {{ break; }}", Identf::V_LINE)?;
        writeln!(f, "      }}")?;
        writeln!(f, "    }}")?;
        writeln!(f, "  }});")?;
    }

    writeln!(f, "  loop {{")?;
    writeln!(f, "    for {} in 0..{}.len() {{", Identf::V_I, Identf::V_COLONIES)?;

    // 標準入力を cin コロニーに送信
    if contains_cin {
        writeln!(f, "      if let Ok({}) = {}.try_recv() {{", Identf::V_LINE, Identf::V_RX)?;
        writeln!(f, "        {}[{}].{}(vec![{}({})]);",
            Identf::V_COLONIES, program_meta.colony_indices[&"cin".to_string()],
            Identf::FN_RECEIVE, Identf::en_type(Type::String), Identf::V_LINE)?;
        writeln!(f, "      }}")?;
    }

    // i 番目の規則を実行, 転送リソースの転送処理
    writeln!(f, "      match {}[{}].{}() {{", Identf::V_COLONIES, Identf::V_I, Identf::FN_RULE)?;
    writeln!(f, "        Ok({}) =>", Identf::V_GIFTS)?;
    writeln!(f, "          for ({}, {}) in {} {{",Identf::V_GIFTS_DEST, Identf::V_GIFTS_VEC,
        Identf::V_GIFTS)?;
    writeln!(f, "            {}[{}].{}({});", Identf::V_COLONIES, Identf::V_GIFTS_DEST,
        Identf::FN_RECEIVE, Identf::V_GIFTS_VEC)?;
    writeln!(f, "          }}")?;
    writeln!(f, "        Err({0}) => return {0}", Identf::V_CODE)?;
    writeln!(f, "      }}")?;

    writeln!(f, "    }}")?; // for V_I in V_COLONIES index
    writeln!(f, "  }}")?; // loop

    writeln!(f, "}}")?; // fn main

    Ok(())
}

fn generate_colony_decl(
    f: &mut impl Write,
    name: &str,
    rules: &Vec<ast::MacroOrRuleSetAST>,
    program_meta: &semantics::ProgramASTMeta,
) -> io::Result<()> {
    let colony_name = Identf::st_colony(name);

    writeln!(f, "struct {} {{", colony_name)?;
    writeln!(f, "  {}: Vec<{}>,", Identf::ME_RESOURCE, Identf::EN_TYPE)?;
    writeln!(f, "}}")?;
    writeln!(f, "impl {} for {} {{", Identf::TR_COLONY, colony_name)?;
    writeln!(f, "  fn {}(&mut self, {}: Vec<{}>) {{ self.{}.extend({}); }}",
        Identf::FN_RECEIVE, Identf::P_GIFTS, Identf::EN_TYPE, Identf::ME_RESOURCE, Identf::P_GIFTS)?;
    writeln!(f, "  fn {}(&mut self) -> Result<HashMap<usize, Vec<{}>>, ExitCode> {{",
        Identf::FN_RULE, Identf::EN_TYPE)?;
    writeln!(f, "    let mut {}: HashMap<usize, Vec<{}>> = HashMap::new();",
        Identf::V_GIFTS, Identf::EN_TYPE)?;
    for rule_set in rules {
        match rule_set {
            ast::MacroOrRuleSetAST::RuleSet(rule_set) => {
                generate_rule_set(f, rule_set, program_meta)?;
            }
            ast::MacroOrRuleSetAST::Macro(m) => {
                generate_macro(f, m)?;
            }
        }
    }
    writeln!(f, "    Ok({})", Identf::V_GIFTS)?;
    writeln!(f, "  }}")?;
    writeln!(f, "}}")?;

    Ok(())
}

fn generate_colony_extension(
    f: &mut impl Write,
    name: &str,
    rules: &Vec<ast::MacroOrRuleSetAST>,
    meta: &Option<semantics::ColonyExtensionASTMeta>,
    program_meta: &semantics::ProgramASTMeta,
) -> io::Result<()> {
    let colony_name = Identf::st_colony(name);

    writeln!(f, "struct {} {{", colony_name)?;
    writeln!(f, "  {}: Vec<{}>,", Identf::ME_RESOURCE, Identf::EN_TYPE)?;
    writeln!(f, "}}")?;
    writeln!(f, "impl {} for {} {{", Identf::TR_COLONY, colony_name)?;
    writeln!(f, "  fn {}(&mut self, {}: Vec<{}>) {{ self.{}.extend({}); }}",
        Identf::FN_RECEIVE, Identf::P_GIFTS, Identf::EN_TYPE, Identf::ME_RESOURCE, Identf::P_GIFTS)?;
    writeln!(f, "  fn {}(&mut self) -> Result<HashMap<usize, Vec<{}>>, ExitCode> {{",
        Identf::FN_RULE, Identf::EN_TYPE)?;
    writeln!(f, "    let mut {}: HashMap<usize, Vec<{}>> = HashMap::new();",
        Identf::V_GIFTS, Identf::EN_TYPE)?;
    for rule_set in rules {
        match rule_set {
            ast::MacroOrRuleSetAST::RuleSet(rule_set) => {
                generate_rule_set(f, rule_set, program_meta)?;
            }
            ast::MacroOrRuleSetAST::Macro(m) => {
                generate_macro(f, m)?;
            }
        }
    }

    // 組み込みコロニーのデフォルト規則
    match meta.as_ref().unwrap().builtin_colony {
        BuiltinColony::Print => {
            writeln!(f, r#"    for resource in &self.resources {{
      println!("{{resource}}");
    }}
    self.resources = vec![];"#)?;
        },
        BuiltinColony::Cin => (),
        BuiltinColony::Cout => {
            writeln!(f, r#"    for resource in &self.resources {{
      print!("{{resource}}");
      io::stdout().flush().unwrap();
    }}
    self.resources = vec![];"#)?;
        },
        BuiltinColony::Exit => {
            writeln!(f, r#"    let mut buf = Vec::new();
    for resource in &self.resources {{
      let mut no_match = true;
      match resource {{
        ResourceType::Int(v) => return Err(ExitCode::from(*v as u8)),
        ResourceType::String(_)|ResourceType::Bool(_)|ResourceType::Double(_)|ResourceType::Symbol(_) => {{}}
      }}
      if no_match {{
        buf.push(resource.clone());
      }}
    }}
    self.resources = buf;"#)?;
        },
    }

    writeln!(f, "    Ok({})", Identf::V_GIFTS)?;
    writeln!(f, "  }}")?; // fn FN_RULE
    writeln!(f, "}}")?; // impl TR_COLONY for st_colony

    Ok(())
}

// MARK: コード生成 - マクロ

fn generate_macro(
    f: &mut impl Write,
    m: &ast::MacroAST,
) -> io::Result<()> {
    match m {
        ast::MacroAST::Debug { message } => {
            writeln!(f, "    println!(\"{} {{:?}}\", self.{});", message, Identf::ME_RESOURCE)?;
        }
        ast::MacroAST::Sleep { duration } => {
            writeln!(f, "    thread::sleep(Duration::from_millis({}));", duration)?;
        }
    }
    Ok(())
}

// MARK: コード生成 - 規則

fn generate_rule_set(
    f: &mut impl Write,
    rule_set: &ast::RuleSetAST,
    program_meta: &semantics::ProgramASTMeta,
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
            generate_multi_condition_rule(f, rule, program_meta)?;
        }
    }

    // メイン: 各リソースを for 文で処理
    writeln!(f, "    let mut {} = Vec::new();", Identf::V_BUF)?;
    writeln!(f, "    for ({}, {}) in self.{}.iter().enumerate() {{",
        Identf::V_I, Identf::V_RSRS_REF, Identf::ME_RESOURCE)?;
    writeln!(f, "      let mut {} = true;", Identf::V_NO_MATCH)?;

    // メイン: 複数条件規則の結果を適用
    if has_multi_cond_rule {
        writeln!(f, "      {} &= !{}[{}];", Identf::V_NO_MATCH, Identf::V_SOME_USED, Identf::V_I)?;
        writeln!(f, "      if let Some({}) = {}.get(&{}) {{",
            Identf::V_IELM_REF, Identf::V_INSERTIONS, Identf::V_I)?;
        writeln!(f, "        {}.extend({}.clone());", Identf::V_BUF, Identf::V_IELM_REF)?;
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
                generate_single_condition_rule(f, rule, t, program_meta)?;
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
    program_meta: &semantics::ProgramASTMeta,
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

        // if condition {
        write!(f, "          if ")?;
        if condition.sqc_start != i {
            write!(f, "{0}[{1}]=={0}[{2}] && ", Identf::V_CAPT_PROG, i, i-1)?;
        }
        generate_multi_condition_judge(f, condition, i, captures, program_meta)?;

        writeln!(f, "            {} = {};", Identf::V_CAPT_IDX, next_idx)?;
        writeln!(f, "            {}[{}[{}]] = true;", Identf::V_USED, Identf::V_CAPT_PROG, i)?;
        writeln!(f, "            {}[{}] += 1;", Identf::V_CAPT_PROG, i)?;
        if is_last {
            // 出力リソースを出力先に push
            let capts_ref_code =
                multi_condition_capts_ref_code(&rule.conditions);
            generate_multi_condition_outputs(
                f,
                &rule.outputs,
                captures,
                &capts_ref_code,
                program_meta,
            )?;
        }
        if condition.sqc_end != i {
            writeln!(f, "            {0}[{1}] = {0}[{2}];", Identf::V_CAPT_PROG, i+1, i)?;
        }
        writeln!(f, "          }} else {{")?;
        if condition.sqc_start != i {
            writeln!(f, "            {} = {};", Identf::V_CAPT_IDX, condition.sqc_start)?;
            writeln!(f, "            for {} in {}..{} {{", Identf::V_I, condition.sqc_start, i)?;
            writeln!(f, "              {}[{}[{}]-1] = false;",
                Identf::V_USED, Identf::V_CAPT_PROG, Identf::V_I)?;
            writeln!(f, "            }}")?;
        }
        writeln!(f, "            {}[{}] += 1;", Identf::V_CAPT_PROG, i)?;
        writeln!(f, "          }}")?; // if condition
        writeln!(f, "        }},")?;
    }
    writeln!(f, "        _ => unreachable!()")?;
    writeln!(f, "      }}")?;

    writeln!(f, "    }}")?; // end while

    // 使用済みフラグを立てたが, 最後の条件式までリソースが揃わなかったものを解除
    writeln!(f, "    for {} in 0..{} {{", Identf::V_I, Identf::V_CAPT_IDX)?;
    writeln!(f, "      {}[{}[{}]-1] = false;", Identf::V_USED, Identf::V_CAPT_PROG, Identf::V_I)?;
    writeln!(f, "    }}")?;

    // V_SOME_USED に使用済みフラグを反映
    writeln!(f, "    for {} in 0..self.{}.len() {{", Identf::V_I, Identf::ME_RESOURCE)?;
    writeln!(f, "      {0}[{2}] |= {1}[{2}];", Identf::V_SOME_USED, Identf::V_USED, Identf::V_I)?;
    writeln!(f, "    }}")?;

    Ok(())
}

/// 複数条件規則の前処理の `if condition {` の `condition {` の 部分を生成
fn generate_multi_condition_judge(
    f: &mut impl Write,
    condition: &ast::ConditionAST,
    cond_idx: usize,
    captures: &HashMap<String, TypeHint>,
    program_meta: &semantics::ProgramASTMeta,
) -> io::Result<()> {
    writeln!(f, "match &self.{}[{}[{}]] {{",
            Identf::ME_RESOURCE, Identf::V_CAPT_PROG, cond_idx)?;

    match &condition.meta.as_ref().unwrap().kind {
        ConditionKind::Equal(_) => {
            let (result_type, expr_str) = {
                let mut buffer = Vec::new();
                let result_type = generate_expr(&mut buffer, &condition.expr, &|_, _| {
                    unreachable!()
                }, program_meta)?;
                (result_type, String::from_utf8(buffer).unwrap())
            };
            writeln!(f, "            {}({}) => {}.clone() == {},", Identf::en_type(result_type),
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
                    write!(f, "{}.clone()", Identf::V_VALUE_REF)?;
                    Ok(*t)
                }, program_meta)?;
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
    program_meta: &semantics::ProgramASTMeta,
) -> io::Result<()> {
    let mut prev_cindex: Option<Option<usize>> = None;
    for output in outputs {
        // 出力先コロニーのインデックス
        let cindex: Option<usize> = output.destination.as_ref().map(|dest| {
            program_meta.colony_indices[dest]
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
        generate_multi_condition_output(f, output, captures, &capts_ref_code, program_meta)?;

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
    program_meta: &semantics::ProgramASTMeta,
) -> io::Result<()> {
    /// 出力リソース push 部分のコードを生成
    fn _generate_push_resource(
        f: &mut impl Write,
        output_expr: &ast::ExprAST,
        generate_capture: &impl Fn(&mut dyn Write, &String) -> io::Result<Type>,
        program_meta: &semantics::ProgramASTMeta,
    ) -> io::Result<()> {
        let (result_type, expr_str) = {
            let mut buffer = Vec::new();
            let result_type = generate_expr(&mut buffer, output_expr, generate_capture, program_meta)?;
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
        program_meta: &semantics::ProgramASTMeta,
    ) -> io::Result<()> {
        if assoc_captures.is_empty() {
            write!(f, "              (")?;
            for capt in assoc_capts {
                write!(f, "{}({}),", Identf::en_type(types[capt]), Identf::v_value_ref(capt))?;
            }
            writeln!(f, ") =>")?;
            write!(f, "                ")?;
            _generate_push_resource(f, output_expr, &|f, name| {
                write!(f, "{}.clone()", Identf::v_value_ref(name))?;
                Ok(types[name])
            }, program_meta)?;
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
                backtrack_types(f, &assoc_captures, &types, output_expr, assoc_capts, program_meta)?; // 再帰
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
        }, program_meta)?;
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
    backtrack_types(f, &assoc_captures, &types, &output.expr, assoc_capts, program_meta)?;

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
    program_meta: &semantics::ProgramASTMeta,
) -> io::Result<()> {
    let cond = &rule.conditions[0];
    let cond_meta = cond.meta.as_ref().unwrap();

    // if condition { 部分を生成
    match cond_meta.kind {
        // 条件式と一致したら出力式を push
        ConditionKind::Equal(_) => {
            let (result_type, expr_str) = {
                let mut buffer = Vec::new();
                let result_type = generate_expr(&mut buffer, &cond.expr, &|_, _| {
                    unreachable!()
                }, program_meta)?;
                (result_type, String::from_utf8(buffer).unwrap())
            };
            if result_type != capture_type { return Ok(()); }
            write!(f, "          if {}.clone() == {} ", Identf::V_VALUE_REF, expr_str)?;
        },
        // 無条件で出力式を push
        ConditionKind::Capture(_) => {
            write!(f, "          ")?;
        },
        // 条件式を満たすならば出力式を push
        ConditionKind::CaptureCondition(_) => {
            write!(f, "          if ")?;
            generate_expr(f, &cond.expr, &|f, _| {
                write!(f, "{}.clone()", Identf::V_VALUE_REF)?;
                Ok(capture_type)
            }, program_meta)?;
            write!(f, " ")?;
        },
    }
    writeln!(f, "{{")?;

    // 出力先にリソースを push する部分を生成
    let mut prev_cindex: Option<usize> = None;
    for output in &rule.outputs {
        // 出力先コロニーのインデックス
        let cindex = output.destination.as_ref().map(|dest| {
            program_meta.colony_indices[dest]
        });
        // 出力先エントリの取り出し (直前に同じエントリを取り出したならスキップ)
        if let Some(index) = cindex {
            if prev_cindex != cindex {
                writeln!(f, "            let {} = {}.entry({}).or_default();",
                    Identf::V_ENTRY_MUT, Identf::V_GIFTS, index)?;
            }
            write!(f, "            {}.push(", Identf::V_ENTRY_MUT)?;
        } else {
            write!(f, "            {}.push(", Identf::V_BUF)?;
        }
        // リソース出力
        generate_resource(f, &output.expr, &|f, _| {
            write!(f, "{}.clone()", Identf::V_VALUE_REF)?;
            Ok(capture_type)
        }, program_meta)?;
        writeln!(f, ");")?;

        if cindex.is_some() {
            prev_cindex = cindex;
        }
    }
    writeln!(f, "            {} = false;", Identf::V_NO_MATCH)?;

    writeln!(f, "          }}")?; // if condition
    Ok(())
}

// MARK: コード生成 - 式

fn generate_resource(
    f: &mut impl Write,
    expr: &ast::ExprAST,
    generate_capture: &impl Fn(&mut dyn Write, &String) -> io::Result<Type>,
    program_meta: &semantics::ProgramASTMeta,
) -> io::Result<()> {
    let (result_type, expr_str) = {
        let mut buffer = Vec::new();
        let result_type = generate_expr(&mut buffer, expr, generate_capture, program_meta)?;
        (result_type, String::from_utf8(buffer).unwrap())
    };
    write!(f, "{}({})", Identf::en_type(result_type), expr_str)?;
    Ok(())
}

fn generate_expr(
    f: &mut impl Write,
    expr: &ast::ExprAST,
    generate_capture: &impl Fn(&mut dyn Write, &String) -> io::Result<Type>,
    program_meta: &semantics::ProgramASTMeta,
) -> io::Result<Type> {
    match expr {
        ast::ExprAST::Number(i) => {
            write!(f, "({}{})", i, Type::actual(&Type::Int))?;
            Ok(Type::Int)
        }
        ast::ExprAST::Double(d) => {
            write!(f, "({:?}{})", d, Type::actual(&Type::Double))?;
            Ok(Type::Double)
        }
        ast::ExprAST::Str(s) => {
            write!(f, "{:?}.to_owned()", s)?;
            Ok(Type::String)
        }
        ast::ExprAST::Bool(b) => {
            write!(f, "{}", b)?;
            Ok(Type::Bool)
        }
        ast::ExprAST::Symbol(name) => {
            let value = program_meta.symbol_values[name];
            write!(f, "{}", value)?;
            Ok(Type::Symbol)
        }
        ast::ExprAST::Capture(name, _) => {
            generate_capture(f, name)
        }
        ast::ExprAST::UnaryOp(opcode, operand, _) => {
            let (operand_type, operand_code) = {
                let mut buffer = Vec::new();
                let operand_type = generate_expr(&mut buffer, operand, generate_capture, program_meta)?;
                (operand_type, String::from_utf8(buffer).unwrap())
            };

            match (opcode, operand_type) {
                (UnaryOpcode::Neg, _) => write!(f, "(-{})", operand_code)?,
                (UnaryOpcode::LogicalNot, _) => write!(f, "(!{})", operand_code)?,
                (UnaryOpcode::BitNot, _) => write!(f, "(!{})", operand_code)?,
                (UnaryOpcode::As(_), _) => write!(f, "{}", operand_code)?,

                (UnaryOpcode::ToInt, Type::String) =>
                    write!(f, "({}.parse::<i32>().unwrap_or_default())", operand_code)?,
                (UnaryOpcode::ToInt, _) => write!(f, "({} as i32)", operand_code)?,

                (UnaryOpcode::ToDouble, Type::String) =>
                    write!(f, "({}.parse::<f64>().unwrap_or_default())", operand_code)?,
                (UnaryOpcode::ToDouble, _) => write!(f, "({} as f64)", operand_code)?,

                (UnaryOpcode::ToString, _) => write!(f, "({}.to_string())", operand_code)?,
                (UnaryOpcode::UtilCeil, _) => write!(f, "({}.ceil()as i32)", operand_code)?,
                (UnaryOpcode::UtilFloor, _) => write!(f, "({}.floor()as i32)", operand_code)?,
                (UnaryOpcode::UtilRound, _) => write!(f, "({}.round()as i32)", operand_code)?,
                (UnaryOpcode::UtilAbs, _) => write!(f, "({}.abs())", operand_code)?,
                (UnaryOpcode::UtilOrd, _) => write!(f, "ord({})", operand_code)?,
                (UnaryOpcode::UtilChr, _) => write!(f, "chr({})", operand_code)?,
                (UnaryOpcode::UtilLen, _) => write!(f, "({}.len()as i32)", operand_code)?,
            }

            match opcode.result_type(operand_type) {
                Some(t) => Ok(t),
                None => panic!("不正な型の演算です: {:?} {:?}", opcode, operand_type),
            }
        }
        ast::ExprAST::BinaryOp(lhs, opcode, rhs, _) => {
            let (lhs_type, mut lhs_code) = {
                let mut buffer = Vec::new();
                let lhs_type = generate_expr(&mut buffer, lhs, generate_capture, program_meta)?;
                (lhs_type, String::from_utf8(buffer).unwrap())
            };
            let (rhs_type, mut rhs_code) = {
                let mut buffer = Vec::new();
                let rhs_type = generate_expr(&mut buffer, rhs, generate_capture, program_meta)?;
                (rhs_type, String::from_utf8(buffer).unwrap())
            };

            write!(f, "(",)?;
            match (lhs_type, rhs_type) {
                (Type::Int, Type::Double) => lhs_code = format!("({} as f64)", lhs_code),
                (Type::Double, Type::Int) => rhs_code = format!("({} as f64)", rhs_code),
                (_, _) => (),
            }
            match (opcode, lhs_type, rhs_type) {
                (Opcode::Mul, _, _) => write!(f, "{}*{}", lhs_code, rhs_code)?,
                (Opcode::Div, _, _) => write!(f, "{}/{}", lhs_code, rhs_code)?,
                (Opcode::Mod, _, _) => write!(f, "{}%{}", lhs_code, rhs_code)?,
                (Opcode::Add, Type::String, _) | (Opcode::Add, _, Type::String) =>
                    write!(f, "format!(\"{{}}{{}}\",{},{})", lhs_code, rhs_code)?,
                (Opcode::Add, _, _) => write!(f, "{}+{}", lhs_code, rhs_code)?,
                (Opcode::Sub, _, _) => write!(f, "{}-{}", lhs_code, rhs_code)?,
                (Opcode::BitAnd, _, _) => write!(f, "{}&{}", lhs_code, rhs_code)?,
                (Opcode::BitOr, _, _) => write!(f, "{}|{}", lhs_code, rhs_code)?,
                (Opcode::BitXor, _, _) => write!(f, "{}^{}", lhs_code, rhs_code)?,
                (Opcode::BitShiftLeft, _, _) => write!(f, "{}<<{}", lhs_code, rhs_code)?,
                (Opcode::BitShiftRight, _, _) => write!(f, "{}>>{}", lhs_code, rhs_code)?,
                (Opcode::LogicalAnd, _, _) => write!(f, "{}&&{}", lhs_code, rhs_code)?,
                (Opcode::LogicalOr, _, _) => write!(f, "{}||{}", lhs_code, rhs_code)?,
                (Opcode::Eq, _, _) => write!(f, "{}=={}", lhs_code, rhs_code)?,
                (Opcode::Ne, _, _) => write!(f, "{}!={}", lhs_code, rhs_code)?,
                (Opcode::Lt, _, _) => write!(f, "{}<{}", lhs_code, rhs_code)?,
                (Opcode::Gt, _, _) => write!(f, "{}>{}", lhs_code, rhs_code)?,
                (Opcode::Le, _, _) => write!(f, "{}<={}", lhs_code, rhs_code)?,
                (Opcode::Ge, _, _) => write!(f, "{}>={}", lhs_code, rhs_code)?,
                (Opcode::Nth, _, _) => write!(f, "nth({},{})", lhs_code, rhs_code)?,
            }
            write!(f, ")",)?;

            match opcode.result_type(lhs_type, rhs_type) {
                Some(t) => Ok(t),
                None => panic!("不正な型の演算です: {:?} {:?} {:?}", lhs_type, opcode, rhs_type),
            }
        }
        ast::ExprAST::Slice(s, start, end, step, _) => {
            let s_code = {
                let mut buffer = Vec::new();
                generate_expr(&mut buffer, s, generate_capture, program_meta)?;
                String::from_utf8(buffer).unwrap()
            };
            let start_code = if let Some(start) = start {
                let mut buffer = Vec::new();
                write!(buffer, "Some(")?;
                generate_expr(&mut buffer, start, generate_capture, program_meta)?;
                write!(buffer, ")")?;
                String::from_utf8(buffer).unwrap()
            } else {
                "None".to_string()
            };
            let end_code = if let Some(end) = end {
                let mut buffer = Vec::new();
                write!(buffer, "Some(")?;
                generate_expr(&mut buffer, end, generate_capture, program_meta)?;
                write!(buffer, ")")?;
                String::from_utf8(buffer).unwrap()
            } else {
                "None".to_string()
            };
            let step_code = if let Some(step) = step {
                let mut buffer = Vec::new();
                write!(buffer, "Some(")?;
                generate_expr(&mut buffer, step, generate_capture, program_meta)?;
                write!(buffer, ")")?;
                String::from_utf8(buffer).unwrap()
            } else {
                "None".to_string()
            };

            write!(f, "{}(&{},{},{},{})", Identf::FN_UTIL_SLICE, s_code, start_code, end_code, step_code)?;
            Ok(Type::String)
        }
    }
}
