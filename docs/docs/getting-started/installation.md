# インストール

Shol言語をインストールして、開発を始めましょう。

## 必要な環境

- Rust 1.xx以上
- Cargo

## インストール手順

1. リポジトリのクローン:
```bash
git clone https://github.com/yourusername/shol.git
```

2. ビルド:
```bash
cd shol
cargo build --release
```

3. インストール:
```bash
cargo install --path .
```

## 動作確認

インストールが完了したら、以下のコマンドで動作確認をしましょう：

```bash
shol --version
```

正しくインストールされていれば、バージョン情報が表示されます。

## 次のステップ

インストールが完了したら、[クイックスタート](quick-start)に進んで基本的な使い方を学びましょう。 