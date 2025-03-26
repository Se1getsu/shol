<img height="120" alt="Sholのロゴ画像" src="docs/static/img/logo.png">

# shol

最高に面白い独自パラダイム言語 Shol のコンパイラです。

Shol は、規則やデータフローを中心にプログラムを表現するルールベースの宣言型言語です。

## インストール

1. [Releases](https://github.com/Se1getsu/shol/releases) から、`shol-<Sholバージョン名>-<あなたの環境>.zip` をダウンロードし、解凍してください。

2. 解凍して得られた `shol.exe` または `shol` をパスの通る場所に置いてください。

   - Windows/Mac/Ubuntu でのパスの通し方 (ChatGPT の回答)\
   https://chatgpt.com/share/67e2d425-b208-800e-bfa0-1e7f7e77c5a0

   - Mac で「"shol" は開いていません」のポップアップが出た場合\
   https://www.century.co.jp/support/faq/macos-error.html

3. `rustc` コマンドを使用可能にするため、以下のサイトから Rust をインストールします。\
   https://www.rust-lang.org/ja/tools/install

4. `rustc --version` と `shol --version` を実行し、それぞれのバージョンが表示されることを確認してください。


## パラダイムの概略

以下は、Shol に登場する概念を、オブジェクト指向の概念と対比させた文章です。

**オブジェクト指向**：*オブジェクト* は *属性* とそれを操作する *メソッド* を持ちます。\
**Shol**：*コロニー* は *リソースキュー* とそれを変化させる *規則* を持ちます。

**オブジェクト指向**：*オブジェクト* は外部からの *メソッド呼び出し* をトリガーに**受動的に**属性を変化させます。\
**Shol**：*コロニー* は *サイクル* ごとに規則を適用し**能動的に**リソースキューを変化させます。

Shol の特徴として、main メソッドのようなエントリーポイントや、if や for といった制御構文が存在しない点が挙げられます。

さらに詳しく学びたい方は、[チュートリアル](https://se1getsu.github.io/shol/docs/tutorial/intro)をお読みください。

## サンプルコード

いくつかのサンプルコードを [`example/`](example/) に置いています。

- [`hello_world.shol`](example/hello_world.shol)\
  「Hello, world!」を出力します。

- [`fizzbuzz.shol`](example/fizzbuzz.shol)\
  入力された数字までの FizzBuzz を出力します。

- [`paren.shol`](example/paren.shol)\
  長さ $N$ の正しい括弧列を辞書順で出力するプログラムです。\
  AtCoder での提出で、合格判定(AC)を受けています：[提出 #64189632 - 競プロ典型 90 問](https://atcoder.jp/contests/typical90/submissions/64189632)

## 貢献

Issue や Pull Request は自由に投げてもらっても OK です。

## リンク集

- **チュートリアル**：初めての方向け\
  https://se1getsu.github.io/shol/docs/tutorial/intro

- **言語リファレンス**：チュートリアルを読んだ方向け\
  https://se1getsu.github.io/shol/docs/reference/intro

- **Shol コミュニティ (Discord)**：Shol に関する話題はもちろん、質問・バグ報告なども受け付けています。\
  https://discord.gg/CrsZKZQeWT
