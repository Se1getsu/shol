use std::collections::{HashMap, HashSet};
use crate::ast;

// MARK: メタデータ

pub struct RuleASTMeta {
    pub captures: HashMap<String, TypeHint>,
}

pub struct TypeHint {
    pub possible_types: HashSet<Type>,
}

pub enum Type {
    Int,
    String,
}

// MARK: 意味解析処理

pub fn analyze_program(_program: &mut Vec<ast::StatementAST>) {
    todo!()
}
