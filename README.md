# EMACS_OBSIDIAN

Emacs 内で Obsidian のような Markdown ノート環境を使うパッケージです。`M-x obsidian` の1コマンドで、左にファイル階層、中央に編集画面、右にリンクグラフを開きます。

初めて使う方は、[日本語ユーザーガイド](docs/USER_GUIDE_JA.md)を参照してください。

## 主な機能

- `[[note]]` / `[[note|表示名]]` 形式の Wiki リンク
- ファイル保存時、存在しないリンク先ノートを同じフォルダに自動作成
- `$...$` と `$$...$$` の LaTeX プレビュー
- 斥力・バネ引力・中心重力を使った force-directed グラフ
- ラベルを保護し、Unicode罫線`─ │ ┌ ┐ └ ┘ ┼`で直交接続を描画
- ツリー、Wiki リンク、グラフのファイル名をマウスで開く操作
- 新規ノートへの作成日時の自動挿入
- ツリー／グラフ幅の保存と復元
- フォルダ単位のグラフスコープ（別フォルダのノートを混在させない）

## インストール

このリポジトリを任意の場所へ置き、`init.el` に次を追加します。

```elisp
(add-to-list 'load-path "/path/to/EMACS_OBSIDIAN")
(require 'obsidian)

;; 毎回フォルダを尋ねずに起動する場合
(setq obsidian-vault-directory "~/Documents/MyVault")
```

設定を評価するか Emacs を再起動し、`M-x obsidian` を実行してください。`obsidian-vault-directory` が `nil` の場合は vault フォルダを尋ねます。

同梱デモを開く場合は、起動時にこのリポジトリ内の `examples/demo-vault` を選びます。左の `music/` をクリックすると、仕様の2ハブ構造を確認できます。

## 基本操作

起動直後のカーソルは左の階層ビューにあります。

### 左：階層ビュー

| 操作 | 内容 |
|---|---|
| マウス左クリック / `RET` | フォルダを開閉、またはノートを中央に開く |
| `TAB` / `→` | フォルダを展開 |
| `←` | フォルダを折りたたむ |
| `n` | 新しいノートを作る |
| `g` / `r` | ツリーを更新 |
| `<` / `>` | 左パネルを縮小／拡大 |

フォルダを選ぶと、そのフォルダが右グラフのスコープになります。ノートを選ぶと、そのノートがあるフォルダがスコープになります。

### 中央：Markdown 編集

| 操作 | 内容 |
|---|---|
| `C-c o n` | 新規ノート |
| `C-c o l` | Wiki リンクを挿入 |
| `M-RET` / `C-c o f` | カーソル位置のリンクを開く |
| マウス左クリック | Wiki リンクを開く |
| `C-c o b` | 直前のノートへ戻る |
| `C-c o t` | LaTeX プレビューを切り替える |
| `C-c o g` | グラフを更新 |
| `C-c o r` | ツリーを更新 |
| `C-x C-s` | 保存。未作成のリンク先をこの時点で作成 |

自動作成を保存時だけに限定しているため、`[[apple]]` の入力途中に `a.md`、`ap.md` などが量産されたり、日本語入力が重くなったりしません。

### 右：グラフビュー

| 操作 | 内容 |
|---|---|
| 矢印キー / `h j k l` | 仮想グラフ上のカメラを移動（パン） |
| マウス左クリック / `RET` | ラベルのノートを中央に開く |
| `g` | 再描画 |
| `0` | 現在ノートを画面中央へ戻す |
| `<` / `>` | 右パネルを縮小／拡大 |

`◆ name` は現在のノート、`● name` はほかのノートです（`.md`は省略）。現在ノートは赤、直接接続されたノートは青、孤立ノートは灰色で表示されます。12ノード以下は右ペイン内へ自動フィットし、それより大きなグラフは矢印キーでゲームのマップのように移動できます。

## LaTeX

`latex` と `dvipng` が PATH 上にあれば数式を画像表示します。見つからない場合は、`e^{i\\pi}`を`e⁽ⁱπ⁾`のような読みやすいUnicode数式へ変換して表示します。

## カスタマイズ

`M-x customize-group RET obsidian RET` から変更できます。主な変数は次の通りです。

- `obsidian-vault-directory`
- `obsidian-tree-width`
- `obsidian-graph-width`
- `obsidian-graph-max-nodes`
- `obsidian-auto-timestamp`
- `obsidian-timestamp-format`
- `obsidian-save-window-sizes`

## テスト

```sh
emacs -Q --batch -L . -L test -l test/obsidian-test.el -f ert-run-tests-batch-and-exit
```

## ライセンス

GPL-3.0
