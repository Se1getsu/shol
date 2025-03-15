use regex::Regex;

pub fn preprocess(program: &str) -> String {
    let mut program = program.to_string();
    program = append_trailing_new_line(&program);
    program = merge_continued_lines(&program);
    program = add_double_quote(&program);
    program
}

/// プログラムの末尾に改行がない場合は追加する (構文解析の都合上)
fn append_trailing_new_line(program: &str) -> String {
    let mut program = program.to_string();
    if !program.ends_with('\n') {
        program.push('\n');
    }
    program
}

/// バックスラッシュで分割された行を 1 行にまとめる
fn merge_continued_lines(program: &str) -> String {
    // program を行ごとに分割
    let mut lines = program
        .split('\n')
        .map(|s| s.to_string())
        .collect::<Vec<String>>();

    /// 削除対象の行かを判定
    fn is_remove_target(line: &String) -> bool {
        let trailing_backslashes = line
            .chars()
            .rev()
            .take_while(|&c| c == '\\')
            .count();
        trailing_backslashes % 2 == 1
    }

    // 逆順に 1 行ずつ判定
    for i in (0..lines.len() - 1).rev() {
        if is_remove_target(&lines[i]) {
            // 2 行を line[i] にまとめて line[i+1] を空行にする
            lines[i] = {
                let mut line = lines[i][..lines[i].len() - 1].to_string();
                line.push_str(&lines[i+1].trim_start());
                line
            };
            lines[i+1].clear(); // 空行のまま残すのは, コンパイルエラーで行番号がずれると分かりにくいため
        }
    }

    lines.join("\n")
}

/// ダブルクォートが省略された文字列リソース行をダブルクォートで囲む
fn add_double_quote(program: &str) -> String {
    // 以下で始まる行はダブルクォートで囲まない
    let excluding1 = vec![
        r"\+", r"\-", r"\*", r"\/", r"\%",
        r"\.", r"\|", "\"",
        r"[0-9]",
    ];
    // 以下に一致する行はダブルクォートで囲まない
    let excluding2 = vec![
        "true", "false",
    ];

    // 行全体にマッチするパターン: lsp, content, rsp に分割
    let line_pattern = r"(?m)^(?P<lsp>\s*)(?P<content>.*\S)(?P<rsp>\s*$)";
    let line_re = Regex::new(line_pattern).unwrap();

    // content がこれにマッチした場合は除外するパターン
    let exclude_pattern = format!(r"^(?:{})|^(?:{})$",
        excluding1.join("|"), excluding2.join("|"));
    let exclude_re = Regex::new(&exclude_pattern).unwrap();

    // 各行をチェック
    line_re.replace_all(program, |caps: &regex::Captures| {
        let lsp = &caps["lsp"];
        let content = &caps["content"];
        let rsp = &caps["rsp"];

        if exclude_re.is_match(content) {
            format!("{}{}{}", lsp, content, rsp)
        } else {
            // エスケープ処理: " の直前の \ の数が偶数の場合のみエスケープする
            let dq_re = Regex::new(r#"(?P<backslashes>\\*)""#).unwrap();
            let escaped_content = dq_re.replace_all(content, |caps: &regex::Captures| {
                let backslashes = &caps["backslashes"];
                if backslashes.chars().count() % 2 == 0 {
                    format!(r#"{}\""#, backslashes)
                } else {
                    format!(r#"{}""#, backslashes)
                }
            });
            // ダブルクォートで囲む
            format!(r#"{}"{}"{}"#, lsp, escaped_content, rsp)
        }
    }).to_string()
}
