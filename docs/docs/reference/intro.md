# Shol 言語リファレンス

このリファレンスマニュアルでは、Shol 言語の仕様について網羅的に説明しています。

## 文法の記法について

本リファレンスでは、字句や文法を簡易的な [BNF 記法](https://ja.wikipedia.org/wiki/%E3%83%90%E3%83%83%E3%82%AB%E3%82%B9%E3%83%BB%E3%83%8A%E3%82%A6%E3%82%A2%E8%A8%98%E6%B3%95)を用いて説明します。\
例えば、以下は Shol のプログラムを表す BNF 記法です。

```
プログラム ::= 改行* コロニー宣言*
```

- `+` は 1 回以上の繰り返しを表します。
- `*` は 0 回以上の繰り返しを表します。
- `?` は 0 回または 1 回の繰り返しを表します。
- `"0".."9"` は "0" から "9" までのどれか 1 文字を表します。
