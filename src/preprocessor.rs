use regex::Regex;

pub fn preprocess(program: &str) -> String {
    let mut program = program.to_string();
    program = append_trailing_new_line(&program);
    program = add_double_quote(&program);
    program
}

fn append_trailing_new_line(program: &str) -> String {
    let mut program = program.to_string();
    if !program.ends_with('\n') {
        program.push('\n');
    }
    program
}

fn add_double_quote(program: &str) -> String {
    // 以下で始まる行はダブルクォートで囲まない
    let excluding_token = vec![
        r"\+", r"\-", r"\*", r"\/", r"\%",
        r"\.", r"\|",
        r"[0-9]",
    ];

    // 除外トークンのパターンを作成
    let exclude_pattern = format!(r"^(?:{})", excluding_token.join("|"));
    let exclude_re = Regex::new(&exclude_pattern).unwrap();

    // 行全体にマッチするパターン
    let line_pattern = r"(?m)^(?P<lsp>\s*)(?P<content>.*\S)(?P<rsp>\s*$)";
    let line_re = Regex::new(line_pattern).unwrap();

    // 各行をチェックして必要な場合のみダブルクォートで囲む
    line_re.replace_all(program, |caps: &regex::Captures| {
        let lsp = &caps["lsp"];
        let content = &caps["content"];
        let rsp = &caps["rsp"];

        if exclude_re.is_match(content) {
            format!("{}{}{}", lsp, content, rsp)
        } else {
            format!("{}\"{}\"{}", lsp, content, rsp)
        }
    }).to_string()
}
