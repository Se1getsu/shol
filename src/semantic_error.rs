use std::collections::HashSet;
use std::fmt;
use std::ops::Range;
use std::collections::HashMap;
use crate::ast::{UnaryOpcode, Opcode};
use crate::semantics::{Type, TypeHint};
use crate::compile_error::{CompileError, CompileErrorBuilder, ErrorKind};

// MARK: SemanticError

/// 意味解析エラー
pub struct SemanticError(Box<dyn Fn(&str) -> CompileError>);

impl SemanticError {
    /// エラーメッセージを生成
    pub fn build_compile_error(&self, source: &str) -> CompileError {
        (self.0)(source)
    }

    /// コロニーの定義が重複している場合のエラー
    pub fn duplicate_colony_definition(
        name: &str,
        location: &Range<usize>,
        previous_location: &Range<usize>,
    ) -> Self {
        let message = format!("{} コロニーの定義が重複しています。", name);
        let location = location.clone();
        let previous_location = previous_location.clone();
        Self(Box::new(move |source: &str| {
            CompileErrorBuilder::new(source, ErrorKind::Error)
                .header(&message, location.start)
                .location_pointer(&location)
                .line("最初の定義:")
                .location_pointer(&previous_location)
                .build()
        }))
    }

    /// 未定義のコロニーへの出力が存在する場合のエラー
    pub fn undefined_colony(
        name: &str,
        location: &Range<usize>,
    ) -> Self {
        let message = format!("未定義のコロニーへの出力: {}", name);
        let location = location.clone();
        Self(Box::new(move |source: &str| {
            CompileErrorBuilder::new(source, ErrorKind::Error)
                .header(&message, location.start)
                .location_pointer(&location)
                .build()
        }))
    }

    /// ビルトインコロニーの名前が不正な場合のエラー
    pub fn invalid_builtin_colony(
        name: &str,
        location: &Range<usize>,
    ) -> Self {
        let message = format!("組み込みコロニー {} は存在しません。", name);
        let location = location.clone();
        Self(Box::new(move |source: &str| {
            CompileErrorBuilder::new(source, ErrorKind::Error)
                .header(&message, location.start)
                .location_pointer(&location)
                .build()
        }))
    }

    /// キャプチャ名が他の条件式と重複している場合のエラー
    pub fn duplicate_capture_name(
        name: &str,
        capture_location: &Range<usize>,
    ) -> Self {
        let message = format!("キャプチャ名 ${} は既に使用されています。", name);
        let location = capture_location.clone();
        Self(Box::new(move |source: &str| {
            CompileErrorBuilder::new(source, ErrorKind::Error)
                .header(&message, location.start)
                .location_pointer(&location)
                .hint("条件式に他の条件式のキャプチャを含めることはできません。")
                .build()
        }))
    }

    /// 1 つの条件式に複数のキャプチャが含まれている場合のエラー
    pub fn multiple_captures_in_condition(
        capture_names: (&str, &str),
        capture_locations: (&Range<usize>, &Range<usize>),
    ) -> Self {
        let message = format!(
            "条件式に異なる複数のキャプチャが含まれています: ${}, ${}",
            capture_names.0, capture_names.1
        );
        let locations = (
            capture_locations.0.clone(),
            capture_locations.1.clone(),
        );
        Self(Box::new(move |source: &str| {
            CompileErrorBuilder::new(source, ErrorKind::Error)
                .header(
                    &message,
                    locations.1.start,
                )
                .location_pointer(&locations.0)
                .location_pointer(&locations.1)
                .hint("条件式に他の条件式のキャプチャを含めることはできません。")
                .build()
        }))
    }

    /// 未定義のキャプチャが使用されている場合のエラー
    pub fn undefined_capture(
        name: &str,
        location: &Range<usize>
    ) -> Self {
        let message = format!("未定義のキャプチャ ${} が使用されています。", name);
        let location = location.clone();
        Self(Box::new(move |source: &str| {
            CompileErrorBuilder::new(source, ErrorKind::TypeError)
                .header(&message, location.start)
                .location_pointer(&location)
                .build()
        }))
    }

    /// 単項演算子の型エラー
    pub fn type_error_unary(
        opcode: &UnaryOpcode,
        location: &Range<usize>,
        operand_t: Type,
    ) -> Self {
        let message = format!(
            "単項演算子 {} は {} 型のオペランドをサポートしていません。",
            opcode, operand_t
        );
        let location = location.clone();
        Self(Box::new(move |source: &str| {
            CompileErrorBuilder::new(source, ErrorKind::TypeError)
                .header(&message, location.start)
                .location_pointer(&location)
                .build()
        }))
    }

    /// 二項演算子の型エラー
    pub fn type_error_binary(
        opcode: &Opcode,
        location: &Range<usize>,
        lhs_t: Type,
        rhs_t: Type,
    ) -> Self {
        let message = format!(
            "{} の演算はサポートされていません。",
            opcode.format(lhs_t.to_string(), rhs_t.to_string()),
        );
        let location = location.clone();
        Self(Box::new(move |source: &str| {
            CompileErrorBuilder::new(source, ErrorKind::TypeError)
                .header(&message, location.start)
                .location_pointer(&location)
                .build()
        }))
    }

    /// スライスの型エラー
    pub fn type_error_slice(
        operand: SliceOperand,
        operand_t: Type,
        location: &Range<usize>,
    ) -> Self {
        let message = match operand {
            SliceOperand::String => format!(
                "{operand_t} 型のスライス演算はサポートされていません。"
            ),
            SliceOperand::Start => format!(
                "スライス演算 [start:end:step] の start に {operand_t} 型は指定できません。"
            ),
            SliceOperand::End => format!(
                "スライス演算 [start:end:step] の end に {operand_t} 型は指定できません。"
            ),
            SliceOperand::Step => format!(
                "スライス演算 [start:end:step] の step に {operand_t} 型は指定できません。"
            ),
        };
        let location = location.clone();
        Self(Box::new(move |source: &str| {
            CompileErrorBuilder::new(source, ErrorKind::TypeError)
                .header(&message, location.start)
                .location_pointer(&location)
                .build()
        }))
    }

    /// 条件式を評価可能なキャプチャ型が存在しない場合のエラー
    pub fn no_type_matches(
        location: &Range<usize>,
    ) -> Self {
        let message = "この条件式を評価可能なキャプチャ型は存在しません。";
        let location = location.clone();
        Self(Box::new(move |source: &str| {
            CompileErrorBuilder::new(source, ErrorKind::TypeError)
                .header(&message, location.start)
                .location_pointer(&location)
                .build()
        }))
    }

    /// キャプチャ条件式の結果が真偽値にならない場合のエラー
    pub fn not_bool_condition(
        location: &Range<usize>,
    ) -> Self {
        let message = "結果が bool 型にならないキャプチャ条件式はサポートされていません。";
        let location = location.clone();
        Self(Box::new(move |source: &str| {
            CompileErrorBuilder::new(source, ErrorKind::TypeError)
                .header(&message, location.start)
                .location_pointer(&location)
                .build()
        }))
    }

    /// 出力式の型推論に失敗した場合のエラー (単項演算子)
    pub fn type_inference_failed_unary(
        location: &Range<usize>,
        t_operand: &HashSet<Type>,
        opcode: &UnaryOpcode,
        t_result: &HashSet<Type>
    ) -> Self {
        let expr = if opcode.is_prefix() {
            format!("{} {}", opcode, t_operand.format())
        } else {
            format!("{} {}", t_operand.format(), opcode)
        };
        let message = format!(
            "出力式の型推論に失敗しました。\n\
            {} -> {} は解決できません。",
            expr, t_result.format()
        );
        let location = location.clone();
        Self(Box::new(move |source: &str| {
            CompileErrorBuilder::new(source, ErrorKind::TypeError)
                .header(&message, location.start)
                .location_pointer(&location)
                .build()
        }))
    }

    /// 出力式の型推論に失敗した場合のエラー (二項演算子)
    pub fn type_inference_failed_binary(
        location: &Range<usize>,
        t_lhs: &HashSet<Type>,
        opcode: &Opcode,
        t_rhs: &HashSet<Type>,
        t_result: &HashSet<Type>
    ) -> Self {
        let message = format!(
            "出力式の型推論に失敗しました。\n\
            {} -> {} は解決できません。",
            opcode.format(t_lhs.format(), t_rhs.format()), t_result.format()
        );
        let location = location.clone();
        Self(Box::new(move |source: &str| {
            CompileErrorBuilder::new(source, ErrorKind::TypeError)
                .header(&message, location.start)
                .location_pointer(&location)
                .build()
        }))
    }

    /// 出力式の型推論に失敗した場合のエラー (スライス演算)
    pub fn type_inference_failed_slice(
        operand: SliceOperand,
        operand_t: HashSet<Type>,
        location: &Range<usize>,
    ) -> Self {
        let operand_t = operand_t.format();
        let message = match operand {
            SliceOperand::String => format!(
                "出力式の型推論に失敗しました。\n\
                {operand_t} 型のスライス演算はサポートされていません。"
            ),
            SliceOperand::Start => format!(
                "出力式の型推論に失敗しました。\n\
                スライス演算 [start:end:step] の start に {operand_t} 型は指定できません。"
            ),
            SliceOperand::End => format!(
                "出力式の型推論に失敗しました。\n\
                スライス演算 [start:end:step] の end に {operand_t} 型は指定できません。"
            ),
            SliceOperand::Step => format!(
                "出力式の型推論に失敗しました。\n\
                スライス演算 [start:end:step] の step に {operand_t} 型は指定できません。"
            ),
        };
        let location = location.clone();
        Self(Box::new(move |source: &str| {
            CompileErrorBuilder::new(source, ErrorKind::TypeError)
                .header(&message, location.start)
                .location_pointer(&location)
                .build()
        }))
    }

    /// 出力式にキャプチャ間の型制約関係が含まれる場合のエラー
    pub fn type_constraint_detected(
        captures: &HashMap<String, TypeHint>,
        associated_captures: &Vec<String>,
        failed_captures_type: &HashMap<String, Type>,
        failed_opcode: &Opcode,
        failed_location: &Range<usize>,
    ) -> Self {
        let message = {
            let possible_types = associated_captures
                .iter()
                .map(|name| {
                    format!(
                        "  ${}:{}",
                        name,
                        captures[name].possible_types.format()
                    )
                })
                .collect::<Vec<String>>()
                .join("\n");
            let failed_types = associated_captures
                .iter()
                .map(|name| {
                    format!(
                        "${}:{}",
                        name,
                        failed_captures_type[name]
                    )
                })
                .collect::<Vec<String>>()
                .join(", ");
            format!(
                "キャプチャ間の型制約関係を検出しました。\n\
                各キャプチャは以下の型を取り得ます。\n\
                {}\n\
                しかし {} のとき、以下の {} 演算が行えません。",
                possible_types, failed_types, failed_opcode
            )
        };
        let location = failed_location.clone();
        Self(Box::new(move |source: &str| {
            CompileErrorBuilder::new(source, ErrorKind::TypeError)
                .header(&message, location.start)
                .location_pointer(&location)
                .hint("型ヒント演算子を用いてキャプチャの型を絞ることで、このエラーを解消できます。")
                .build()
        }))
    }
}

// MARK: SliceOperand

/// スライス演算のオペランドの種類
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum SliceOperand {
    String,
    Start,
    End,
    Step,
}

// MARK: UnaryOpcode::is_prefix

impl UnaryOpcode {
    /// 前置演算子かどうかを返す
    pub fn is_prefix(&self) -> bool {
        match self {
            UnaryOpcode::Neg
            | UnaryOpcode::LogicalNot
            | UnaryOpcode::BitNot
            => true,
            UnaryOpcode::As(_)
            | UnaryOpcode::ToInt
            | UnaryOpcode::ToDouble
            | UnaryOpcode::ToString
            | UnaryOpcode::UtilCeil
            | UnaryOpcode::UtilFloor
            | UnaryOpcode::UtilRound
            | UnaryOpcode::UtilAbs
            | UnaryOpcode::UtilOrd
            | UnaryOpcode::UtilChr
            | UnaryOpcode::UtilLen
            => false,
        }
    }
}

// MARK: Format

trait Format {
    fn format(&self) -> String;
}

impl Format for HashSet<Type> {
    fn format(&self) -> String {
        let mut type_strings: Vec<String> = self
            .iter()
            .map(|t| t.to_string())
            .collect();
        type_strings.sort();

        let mut buf = String::new();
        buf.push_str("{");
        buf.push_str(&type_strings.join(", "));
        buf.push_str("}");
        buf
    }
}

// MARK: impl fmt::Display for Operators/Types

impl fmt::Display for UnaryOpcode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            UnaryOpcode::Neg => write!(f, "-"),
            UnaryOpcode::LogicalNot => write!(f, "!"),
            UnaryOpcode::BitNot => write!(f, "~"),
            UnaryOpcode::As(t) => write!(f, ":{}", t),
            UnaryOpcode::ToInt => write!(f, ".int"),
            UnaryOpcode::ToDouble => write!(f, ".double"),
            UnaryOpcode::ToString => write!(f, ".str"),
            UnaryOpcode::UtilCeil => write!(f, ".ceil"),
            UnaryOpcode::UtilFloor => write!(f, ".floor"),
            UnaryOpcode::UtilRound => write!(f, ".round"),
            UnaryOpcode::UtilAbs => write!(f, ".abs"),
            UnaryOpcode::UtilOrd => write!(f, ".ord"),
            UnaryOpcode::UtilChr => write!(f, ".chr"),
            UnaryOpcode::UtilLen => write!(f, ".len"),
        }
    }
}

impl fmt::Display for Opcode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match self {
            Opcode::Mul => "*",
            Opcode::Div => "/",
            Opcode::Mod => "%",
            Opcode::Add => "+",
            Opcode::Sub => "-",
            Opcode::BitAnd => "&",
            Opcode::BitOr => "|",
            Opcode::BitXor => "^",
            Opcode::BitShiftLeft => "<<",
            Opcode::BitShiftRight => ">>",
            Opcode::LogicalAnd => "&&",
            Opcode::LogicalOr => "||",
            Opcode::Eq => "=",
            Opcode::Ne => "!=",
            Opcode::Lt => "<",
            Opcode::Gt => ">",
            Opcode::Le => "<=",
            Opcode::Ge => ">=",
            Opcode::Nth => "[index]",
        })
    }
}

impl Opcode {
    fn format(&self, lhs: String, rhs: String) -> String {
        match self {
            Opcode::Nth => format!("{}[{}]", lhs, rhs),
            _ => format!("{} {} {}", lhs, self, rhs),
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match self {
            Type::Int => "int",
            Type::Double => "double",
            Type::String => "str",
            Type::Bool => "bool",
            Type::Symbol => "symbol",
        })
    }
}
