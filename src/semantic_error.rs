use crate::ast;
use crate::compile_error::{CompileError, CompileErrorBuilder, ErrorKind};

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
            CompileErrorBuilder::new(source, ErrorKind::SemanticError)
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
}
