# クイックスタート

Shol言語の基本的な使い方を学びましょう。

## Hello, World!

まずは伝統的な「Hello, World!」プログラムから始めましょう。

```shol
colony hello {
  rule {
    => "Hello, World!" -> cout
  }
}
```

このコードを`hello.shol`として保存し、以下のコマンドで実行します：

```bash
shol hello.shol
```

## 基本的なプログラムの構造

Shol言語のプログラムは「コロニー」と呼ばれる単位で構成されます。コロニーには「ルール」が含まれ、ルールには条件と出力があります。

### コロニーの宣言

```shol
colony コロニー名 {
  // リソースや規則を定義
}
```

### ルールの定義

```shol
rule {
  条件 => 出力
}
```

## 変数と計算

変数（キャプチャ）を使って計算を行う例：

```shol
colony calculator {
  rule {
    $x:int $y:int => ($x + $y) -> cout
  }
}
```

## プログラムの実行

作成したプログラムは以下のコマンドで実行できます：

```bash
shol プログラム名.shol
```

## 次のステップ

より詳しい文法や機能については、[基本文法](basic-syntax)を参照してください。 