use core::panic;
use std::io::{ self, Write };
use crate::ast::{ self, Opcode };
use crate::semantics::{ Type, OpcodeSignatureExt };

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
struct Identf;
impl Identf {
    /// リソースの型の列挙型
    const EN_TYPE: &'static str = "ResourceType";
    /// コロニーの規則を実行するメソッド
    const FN_RULE: &'static str = "rule";
    /// リソースのメンバ変数
    const ME_RESOURCE: &'static str = "resources";

    /// コロニーの構造体
    fn st_colony(name: &str) -> String {
        format!("Colony_{}", name)
    }
    /// コロニーの構造体のインスタンス
    fn v_colony(name: &str) -> String {
        format!("_{}", name)
    }
    /// EN_TYPE の列挙子
    fn en_type(t: Type) -> String {
        format!("{}::{:?}", Identf::EN_TYPE, t)
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

    // ステートメント
    for stmt in program {
        writeln!(f, "")?;
        match stmt {
            ast::StatementAST::ColonyDecl { name, rules, .. } =>
                generate_colony_decl(f, name, rules)?,
            ast::StatementAST::ColonyExtension { name, rules, .. } =>
                generate_colony_extension(f, name, rules)?,
        }
    }

    // メイン関数
    writeln!(f, "")?;
    writeln!(f, "fn main() {{")?;

    // コロニーの作成と初期化
    for stmt in program {
        match stmt {
            ast::StatementAST::ColonyDecl { name, resources, .. } |
            ast::StatementAST::ColonyExtension { name, resources, .. } => {
                writeln!(f, "  let mut {} = {} {{", Identf::v_colony(name), Identf::st_colony(name))?;
                writeln!(f, "    {}: vec![", Identf::ME_RESOURCE)?;
                for resource in resources {
                    write!(f, "      ")?;
                    generate_resource(f, resource, &|name| {
                        // リソースは文法上リテラルしか許容してないので, このクロージャは呼ばれないはず
                        panic!("リソースにキャプチャが含まれます: ${}", name)
                    })?;
                    writeln!(f, ",")?;
                }
                writeln!(f, "    ],")?;
                writeln!(f, "  }};")?;
            },
        }
    }

    // TODO: 規則の呼び出し

    // メイン関数 ここまで
    writeln!(f, "}}")?;

    Ok(())
}

fn generate_colony_decl(
    f: &mut impl Write,
    name: &str,
    _rules: &Vec<ast::RuleSetAST>,
) -> io::Result<()> {
    let colony_name = Identf::st_colony(name);

    writeln!(f, "struct {} {{", colony_name)?;
    writeln!(f, "  {}: HashMap<String, {}>,", Identf::ME_RESOURCE, Identf::EN_TYPE)?;
    writeln!(f, "}}")?;
    writeln!(f, "impl {} {{", colony_name)?;
    writeln!(f, "  fn {}(&mut self) {{", Identf::FN_RULE)?;
    writeln!(f, "    todo!();")?;
    writeln!(f, "  }}")?;
    writeln!(f, "}}")?;

    Ok(())
}

fn generate_colony_extension(
    f: &mut impl Write,
    name: &str,
    _rules: &Vec<ast::RuleSetAST>,
) -> io::Result<()> {
    let colony_name = Identf::st_colony(name);

    writeln!(f, "struct {} {{", colony_name)?;
    writeln!(f, "  {}: HashMap<String, {}>,", Identf::ME_RESOURCE, Identf::EN_TYPE)?;
    writeln!(f, "}}")?;
    writeln!(f, "impl {} {{", colony_name)?;
    writeln!(f, "  fn {}(&mut self) {{", Identf::FN_RULE)?;
    writeln!(f, "    todo!();")?;
    writeln!(f, "  }}")?;
    writeln!(f, "}}")?;

    Ok(())
}

// MARK: コード生成 - 式

fn generate_resource(
    f: &mut impl Write,
    expr: &ast::ExprAST,
    generate_capture: &impl Fn(&String) -> io::Result<Type>,
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
    generate_capture: &impl Fn(&String) -> io::Result<Type>,
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
            return generate_capture(name);
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
