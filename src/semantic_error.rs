use std::fmt;
use std::ops::Range;
use crate::ast::{self, UnaryOpcode, Opcode};
use crate::semantics::Type;
use crate::compile_error::{CompileError, CompileErrorBuilder, ErrorKind};

// MARK: SemanticError

/// 意味解析エラー
pub struct SemanticError(Box<dyn Fn(&str) -> CompileError>);

impl SemanticError {
    /// エラーメッセージを生成
    pub fn build_compile_error(&self, source: &str) -> CompileError {
        (self.0)(source)
    }

    /// キャプチャ名が他の条件式と重複している場合のエラー
    pub fn duplicate_capture_name(name: String, condition: &ast::ConditionAST) -> Self {
        let location = condition.location.clone();
        Self(Box::new(move |source: &str| {
            CompileErrorBuilder::new(source, ErrorKind::Error)
            .header(
                &format!("キャプチャ名 ${} は既に使用されています。", name),
                location.start,
            )
            .location_pointer(&location)
            .hint(
                "条件式に他の条件式のキャプチャを含めることはできません。",
            )
            .build()
        }))
    }

    /// 単項演算子の型エラー
    pub fn type_error_unary(opcode: &UnaryOpcode, location: &Range<usize>, operand_t: Type) -> Self {
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
    pub fn type_error_binary(opcode: &Opcode, location: &Range<usize>, lhs_t: Type, rhs_t: Type) -> Self {
        let message = format!(
            "{} {} {} の演算はサポートされていません。",
            lhs_t, opcode, rhs_t
        );
        let location = location.clone();
        Self(Box::new(move |source: &str| {
            CompileErrorBuilder::new(source, ErrorKind::TypeError)
                .header(&message, location.start)
                .location_pointer(&location)
                .build()
        }))
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
            Opcode::Eq => "==",
            Opcode::Ne => "!=",
            Opcode::Lt => "<",
            Opcode::Gt => ">",
            Opcode::Le => "<=",
            Opcode::Ge => ">=",
        })
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match self {
            Type::Int => "int",
            Type::Double => "double",
            Type::String => "str",
            Type::Bool => "bool",
        })
    }
}
