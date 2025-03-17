use std::ops::Range;
use std::collections::HashMap;
use std::fmt;

// MARK: CompileError

/// コンパイルエラー
pub struct CompileError(String);

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

// MARK: カラーコード

const RED: &str = "\x1b[1;31m";
const BLUE: &str = "\x1b[1;34m";
const CYAN: &str = "\x1b[1;36m";
const RESET: &str = "\x1b[0m";

// MARK: CompileErrorBuilder

/// コンパイルエラーのビルダー
pub struct CompileErrorBuilder<'src> {
    /// 元のソースコード
    source: &'src str,
    /// エラーの種類
    kind: ErrorKind,
    /// position_to_line_column のキャッシュ
    location_cache: HashMap<usize, (usize, usize)>,
    /// ビルド結果
    error: CompileError,
}

pub enum ErrorKind {
    /// 構文エラー
    SyntaxError,
    /// 型エラー
    TypeError,
    /// エラー (その他のエラー)
    Error,
}

impl<'src> CompileErrorBuilder<'src> {
    pub fn new(source: &'src str, kind: ErrorKind) -> Self {
        Self {
            source,
            kind,
            location_cache: HashMap::new(),
            error: CompileError(String::new()),
        }
    }

    /// エラーヘッダーを追加
    pub fn header(mut self, message: &str, location: usize) -> Self {
        let (line, column) = self.position_to_line_column(location);
        let kind = match self.kind {
            ErrorKind::SyntaxError => "構文エラー",
            ErrorKind::TypeError => "型エラー",
            ErrorKind::Error => "エラー",
        };
        self.error.0.push_str(&format!(
            "{RED}{kind}{RESET}: {message}\n\
            {BLUE}  -->{RESET} {line} 行目 {column} 文字目\n"
        ));
        self
    }

    /// エラー発生箇所を示すパートを追加
    pub fn location_pointer(mut self, location: &Range<usize>) -> Self {
        let (line, column) = self.position_to_line_column(location.start);
        let pointer = format_pointer(column, location.len(), '^');
        let line_content = self.source.lines().nth(line - 1).unwrap_or("");
        let line = line.to_string();
        let space = " ".repeat(line.len());
        self.error.0.push_str(&format!(
            "{BLUE}{space} |\n\
            {line} | {RESET}{line_content}\n\
            {BLUE}{space} | {RESET}{RED}{pointer}{RESET}\n",
        ));
        self
    }

    /// ヒントを追加
    pub fn hint(mut self, message: &str) -> Self {
        self.error.0.push_str(&format!(
            "{CYAN}ヒント{RESET}: {message}\n",
        ));
        self
    }

    /// ビルド結果を取得
    pub fn build(self) -> CompileError {
        self.error
    }

    /// 位置情報を行と列の位置に変換
    fn position_to_line_column(&mut self, pos: usize) -> (usize, usize) {
        if let Some(location) = self.location_cache.get(&pos) {
            *location
        } else {
            let location = self.source
                .chars()
                .take(pos)
                .fold((1, 1), |(line, col), c| {
                    if c == '\n' {
                        (line + 1, 1)
                    } else {
                        (line, col + 1)
                    }
                });
            self.location_cache.insert(pos, location);
            location
        }
    }
}

// MARK: format_pointer

/// エラー発生箇所の矢印を生成する
fn format_pointer(column: usize, length: usize, pointer_char: char) -> String {
    let mut pointer = String::new();
    for _ in 0..column-1 {
        pointer.push(' ');
    }
    for _ in 0..length {
        pointer.push(pointer_char);
    }
    pointer
}
