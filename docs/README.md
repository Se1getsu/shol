# Shol ドキュメントサイト

このディレクトリは [Shol のドキュメントサイト](https://se1getsu.github.io/shol/)のプロジェクトです。

このプロジェクトは [Docusaurus](https://docusaurus.io/) によってビルドされ、`gh-pages` ブランチにビルド結果が push されます。

## Installation

```
$ yarn
```

## Local Development

```
$ yarn start
```

このコマンドは、ローカルの開発用サーバーを起動し、ブラウザウィンドウを開きます。ほとんどの変更はサーバーを再起動することなくライブで反映されます。

## Deployment

Using SSH:

```
$ USE_SSH=true yarn deploy
```

Not using SSH:

```
$ GIT_USER=<Your GitHub username> yarn deploy
```

このコマンドは、プロジェクトのビルドと `gh-pages` ブランチへのプッシュを行います。
