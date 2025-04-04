# 式

## リテラル

```
リテラル ::= bool型リテラル
        | "-"? double型リテラル
        | "-"? int型リテラル
        | str型リテラル
        | 識別子
        | symbol型リテラル
```

`識別子` はダブルクォートが省略された str 型リテラルとして扱われます。

## 演算子の優先順位
<!-- no toc -->
1. [前置修飾の単項演算子](#pre-unary-op)
2. [後置修飾の単項演算子](#post-unary-op)
3. [乗除剰余演算子](#mul-div-mod-op)
4. [加減算演算子](#add-sub-op)
5. [ビットシフト演算子](#bit-shift-op)
6. [比較演算子](#comparison-op)
7. [ビット演算子](#bitwise-op) `&`
8. [ビット演算子](#bitwise-op) `^`
9.  [ビット演算子](#bitwise-op) `|`
10. [論理演算子](#logical-op) `&&`
11. [論理演算子](#logical-op) `||`

## 単項演算子(前置修飾) {#pre-unary-op}

- **`-` int -> int**
- **`-` double -> double**

    符号の反転

- **`!` bool -> bool**

    論理否定

- **`~` int -> int**

    ビット反転

## 単項演算子(後置修飾) {#post-unary-op}

### 型ヒント演算子

型推論時にのみ効果を発揮する演算子。オペランドの値がそのまま結果の値となる。

- **int `:int` -> int**

- **`:i`**：`:int` のエイリアス

- **double `:double` -> double**

- **`:d`**：`:double` のエイリアス

- **str `:str` -> str**

- **`:d`**：`:str` のエイリアス

- **bool `:bool` -> bool**

- **`:b`**：`:bool` のエイリアス

- **symbol `:symbol` -> symbol**

- **`:sym`**：`:sym` のエイリアス

### 型変換演算子

- **double `.int` -> int**

    小数点以下を切り捨てる。0 に近い方向に丸められる。

- **str `.int` -> int**

    数値文字列をパースして数値に変換する。変換に失敗したら `0` となる。

- **bool `.int` -> int**

    `false` は `0` に、`true` は `1` に変換される。

- **`.i`**：`.int` のエイリアス

- **int `.double` -> double**

    整数を浮動小数点数に変換する。

- **str `.double` -> double**

    数値文字列をパースして数値に変換する。変換に失敗したら `0.` となる。

- **bool `.double` -> double**

    `false` は `0.` に、`true` は `1.` に変換される。

- **`.d`**：`.double` のエイリアス

- **int `.str` -> str**
- **double `.str` -> str**

    数値を文字列に変換する。

- **bool `.str` -> str**

    `false` は `"false"` に、`true` は `"true"` に変換される。

- **`.s`**：`.str` のエイリアス

### スライス演算子

Python の添え字・スライスの記法とほぼ同じ仕様です。

- **str `[` int `]` -> str**

    文字列中の 1 文字を取り出す。\
    負の添え字を指定すると、文字列の末尾からの位置となる。\
    範囲外の添え字を指定した場合は、空文字列になる。

    ```shol
    "Shol"[2]  // => "o"
    "Shol"[-2] // => "o"
    "Shol"[4]  // => ""
    ```

- **str `[` int? `:` int? `]` -> str**

    文字列中の部分文字列を `[start:end]` の記法で取り出す。\
    負の添え字を指定すると、文字列の末尾からの位置となる。\
    範囲外の添え字を指定した場合は、範囲内に収められるように調整される。\
    `start` を省略すると、文字列の先頭から取り出される。\
    `end` を省略すると、文字列の末尾まで取り出される。

    ```shol
    "Shol"[1:3]   // => "ho"
    "Shol"[-3:-1] // => "ho"
    "Shol"[1:10]  // => "hol"
    "Shol"[:2]    // => "Sh"
    "Shol"[2:]    // => "ol"
    "Shol"[:]     // => "Shol"
    ```

- **str `[` int? `:` int? `:` int? `]` -> str**

    文字列中の部分文字列を `[start:end:step]` の記法で取り出す。\
    `[start:end]` の範囲から `step` 文字ごとに文字列を取り出す。\
    `step` を省略すると、`1` が指定されたものとして扱われ、`[start:end]` の挙動と同様になる。\
    負のステップ数を指定すると、文字列を逆順に取り出す。

    ```shol
    "Shol"[::2]    // => "Sl"
    "Shol"[::-1]   // => "lohS"
    "Shol"[1:4:2]  // => "hl"
    "Shol"[4::-2]  // => "lh"
    "Shol"[::]     // => "Shol"
    ```

### ユーティリティ演算子

- **double `.ceil` -> int**

    小数点以下を切り上げる。

- **double `.floor` -> int**

    小数点以下を切り捨てる。`.int` と違って、負の無限大方向に丸められる。

- **double `.round` -> int**

    小数点以下を四捨五入する。

- **int `.abs` -> int**
- **double `.abs` -> double**

    絶対値を取る。

- **str `.ord` -> int**

    文字列中の最初の文字の Unicode コードポイントを取得する。文字列が空の場合は `0` となる。

- **int `.chr` -> str**

    Unicode コードポイントを文字列に変換する。不正なコードポイントが指定された場合は空文字列になる。

- **str `.len` -> int**

    文字列の長さを取得する。

## 乗除剰余演算子 {#mul-div-mod-op}

- **int `*` int -> int**
- **int `*` double -> double**
- **double `*` int -> double**
- **double `*` double -> double**

    乗算

    以下も同一の型シグネチャで定義されています。

    - **`/`**：除算

    - **`%`**：剰余

## 加減算演算子 {#add-sub-op}

- **int `+` int -> int**
- **int `+` double -> double**
- **double `+` int -> double**
- **double `+` double -> double**

    加算

- **str `+` str -> str**

    文字列の連結

- **str `+` int -> str**
- **str `+` double -> str**
- **str `+` bool -> str**

    右側のオペランドを文字列に変換して連結する。

- **int `-` int -> int**
- **int `-` double -> double**
- **double `-` int -> double**
- **double `-` double -> double**

    減算

## ビットシフト演算子 {#bit-shift-op}

- **int `<<` int -> int**

    左シフト

- **int `>>` int -> int**

    右シフト

## 比較演算子 {#comparison-op}

- **int `=` int -> bool**
- **int `=` double -> bool**
- **double `=` int -> bool**
- **double `=` double -> bool**
- **str `=` str -> bool**
- **bool `=` bool -> bool**
- **symbol `=` symbol -> bool**

    等価

    以下も同一の型シグネチャで定義されています。

    - **`!=`**：等価でない

- **int `<` int -> bool**
- **int `<` double -> bool**
- **double `<` int -> bool**
- **double `<` double -> bool**

    小なり

    以下も同一の型シグネチャで定義されています。

    - **`>`**：大なり
    - **`<=`**：小なりイコール
    - **`>=`**：大なりイコール

## ビット演算子 {#bitwise-op}

- **int `&` int -> int**
- **bool `&` bool-> bool**

    ビット AND

- **int `^` int -> int**

    ビット XOR

- **int `|` int -> int**
- **bool `|` bool-> bool**

    ビット OR

## 論理演算子 {#logical-op}

- **bool `&&` bool -> bool**

    論理 AND

- **bool `||` bool -> bool**

    論理 OR
