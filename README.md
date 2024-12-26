# shol

最高に面白い独自パラダイム言語 Shol のコンパイラを制作中です。

Shol は、規則やデータフローを中心にプログラムを表現するルールベースの宣言型言語です。

## 用語と概念

- **コロニー**：1 サイクルごとに適用される規則を持ったリソースの集合
- **リソース**：数値や文字列、タプルなどのデータ
- **規則**：コロニーに変化を与える変換規則
- **サイクル**：規則が適用される周期

これらをオブジェクト指向の概念と対比してみましょう。

オブジェクト指向: *オブジェクト* は *属性* とそれを操作する *メソッド* を持ちます。オブジェクトは外部からの *メソッド呼び出し* をトリガーに**受動的に**属性を変化させます。

Shol: *コロニー* は *リソース集合* とそれを変化させる *規則* を持ちます。コロニーは *サイクル* ごとに規則を適用し**能動的に**リソース集合を変化させます。

このような性質から、Shol には Java の main メソッドのようなエントリーポイントは存在しません。

- **並列規則**：直前の規則と並列に実行される規則

1 サイクルの間に、規則は最初に書かれたものから順に逐次的に処理され、各コロニーのリソース集合を変化させます。

## コード例

### Hello, world!

```
%print
Hello, world!
```

外部コロニーである `print` を拡張し、出力する文字列をリソースとして配置すると、出力が行われます。

### FizzBuzz

```
*nGen
1
. $<100 # $+1
| 1 #fizzBuzz $

*fizzBuzz
. $ % 3 = 0 # Fizz
| $ % 5 = 0 # Buzz
. Fizz, Buzz # FizzBuzz
. U #print $

%print
```

`nGen` コロニーは 1〜100 の整数リソースを `fizzBuzz` コロニーに転送します。

`fizzBuzz` コロニーは整数リソースを Fizz や Buzz に変換して標準出力に送ります。

コード中に現れるものが、それぞれどの概念に対応するかを以下に示します。

```
*nGen   // コロニー定義
1       // リソース
. $<100 # $+1       // 規則
| 1 #fizzBuzz $     // 並列規則

*fizzBuzz           // コロニー定義
. $ % 3 = 0 # Fizz  // 規則
| $ % 5 = 0 # Buzz  // 並列規則
. Fizz, Buzz # FizzBuzz  // 規則
. U #print $        // 規則
```

### 関数

```
// (a,b) -> a + b
*add
. U #- $.0 + $.1

// n -> n!
*factorial
. $:int # ($, 1)
. $.0 <= 1 #- $.1
. U # ($.0 - 1, $.1 * $.0)

*send
a
. U #add-factorial-printResult (2, 3)

*printResult
. U #print "(2+3)! = ", $

%print
```

コロニーチェーン `#add-factorial-printResult` はリソースの流れを表しています。
