use crate::ast;
use crate::parser::position_to_line_column;

pub enum SemanticError<'src> {
    /// 以前の条件式とキャプチャ名が重複している
    DuplicateCaptureName {
        /// キャプチャ名
        name: String,
        /// エラーが発生した条件式
        condition: &'src ast::ConditionAST,
    }
}

const RED: &str = "\x1b[1;31m";
const BLUE: &str = "\x1b[1;34m";
const CYAN: &str = "\x1b[1;36m";
const RESET: &str = "\x1b[0m";

impl<'src> SemanticError<'src> {
    pub fn format(&self, source: &str) -> String {
        match self {
            SemanticError::DuplicateCaptureName { name, condition } => {
                let msg = format!("キャプチャ名 ${} は既に使用されています。", name);
                let (line, column) = position_to_line_column(source, condition.location.start);
                let line_content = source.lines().nth(line - 1).unwrap_or("");
                let pointer = format_pointer(column, condition.location.len());
                let line = line.to_string();
                format!(
                    "{RED}エラー{RESET}: {msg}\n\
                    {BLUE}  -->{RESET} {line} 行目 {column} 文字目\n\
                    {BLUE}{space} |\n\
                    {line} | {RESET}{line_content}\n\
                    {BLUE}{space} | {RESET}{pointer}\n\
                    {CYAN}ヒント{RESET}: 条件式に他の条件式のキャプチャを含めることはできません。",
                    space = " ".repeat(line.len()),
                )
            }
        }
    }
}

/// エラー発生箇所の矢印を生成する
fn format_pointer(column: usize, length: usize) -> String {
    let mut pointer = String::new();
    for _ in 0..column-1 {
        pointer.push(' ');
    }
    pointer.push_str("\x1b[1;31m");
    for _ in 0..length {
        pointer.push('^');
    }
    pointer.push_str("\x1b[0m");
    pointer
}
