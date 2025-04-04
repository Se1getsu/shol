# プリプロセス

プリプロセスは、コンパイル前に行われる前処理です。

コンパイルエラー内で表示されるプログラムは、プリプロセス後のプログラムです。

プリプロセス結果は、`-S` オプションを指定することで確認することができます。

```sh
shol -S myscript.shol
```

プリプロセス後のプログラムは、`myscript.shi` に保存されます。

## 処理内容

入力されたソースに、以下の前処理を順に行います。

1. **末尾に改行を追加**

    ファイルの末尾に改行がない場合、改行を追加します。\
    これは構文解析のために、文法の都合上、必要な処理です。

2. **バックスラッシュで分割された行を 1 行に結合**

    バックスラッシュで行が分割されている場合、1 行に結合します。

    コンパイルエラーで行番号がずれるのを防ぐため、結合されると消えてしまう行には空行が挿入されます。\
    例えば、以下のようなプログラムがある場合、

    ```shol
    %print
    "Hello, \
    World!"
    "Hi!"
    ```

    プリプロセス後は以下のようになります。

    ```shol
    %print
    "Hello, World!"

    "Hi!"
    ```

3. **ダブルクォートが省略された文字列リソース行をダブルクォートで囲む**

    ダブルクォートで囲まれていない文字列リソース行を、ダブルクォートで囲みます。

    例えば、以下のようなプログラムがある場合、

    ```shol
    %print
    Hello, World!
    ```

    プリプロセス後は以下のようになります。

    ```shol
    %print
    "Hello, World!"
    ```
