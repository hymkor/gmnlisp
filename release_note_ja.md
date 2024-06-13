v0.5.0
======
Jun 14, 2024

- 浮動小数点型実数の指数表現をサポート
- 整数のフォーマット `#b..`, `#o..`, および `#x..` を追加
- `(ignore-errors FORMS...)` を実装
- `(defclass)`, `(create)` を実装
- gmnlisp.exe: go-multiline-ny v0.12.1 → v0.15.0 - ヒストリ機能を改善

v0.4.1
======
Oct 01 2023

- gmnlisp.exe: SKK変換時に反転(ESC[7m)や下線(ESC[4m)を使うようにした
- gmnlisp.exe: ユーザ辞書が存在しない場合、SKK起動に失敗する不具合を修正

v0.4.0
======
Sep 30 2023

- gmnlisp.exe: go-multiline-ny で複数行編集をサポート
- gmnlisp.exe: SKK (go-readline-skk) での日本語入力をサポート  
  SKKを使用するには
    - (Windows):  `set "GOREADLINESKK=(system-jisyo-paths..);user=(user-jisyo-path)"`  
      for example `set "GOREADLINESKK=~/Share/Etc/SKK-JISYO.*;user=~/.go-skk-jisyo"`
    - (Linux): `export "GOREADLINESKK=(system-jisyo-paths..):user=(user-jisyo-path)"`

v0.3.1
======
Sep 11 2023

- Go言語向けのツール用の型と関数を追加しました。
    - 型 `Dynamics` とメソッド群を追加
    - メソッド  `(*World) NewDynamics` and `(*World) Dynamic`
    - `(dynamic...)` 向けテストコード追加

v0.3.0
======
Jul 29 2023

- 縦棒で囲んだ名前のシンボル名をサポート（ISLisp の仕様）
- スタックトレースの表示フォーマットを変更
- 変数名イタレーターを提供する `(*World) Range(Symbol,Node)`  を実装
- 全変数の名前と値を表示する `(gmn:dump-session)` を実装
- 次のケースでの末尾再帰最適化をサポート
    - `(defun X () .. (X) )`
    - `(defun X () .. (progn (X)) )`
    - `(defun X () .. (if .. (X) (X)))`
    - `(defun X () .. (let (..) .. (X)))`
    - `(defun X () .. (let* (..) .. (X)))`
    - `(defun X () .. (cond ... (t (X))))`
- （format): シーケンス "~X" (X は英大文字) が機能しなかったのを修正
- サブパッケージ "pkg/auto" と "pkg/common" を削除

v0.2.1
======
Jan 29 2023

- `(format FD "~N%")` をサポート
- Go関数 `HasValue` を `IsSome` へ改名
- Go関数 `IsNull` を `IsNone` へ改名
- 以下を実装
    - `(char<)` `(char>)` `(char=)` `(char<=)` `(char>=)` `(char/=)`
    - `(characterp)`
    - `(create-list)`
    - `(char-index)`
    - `(basic-array-p)` `(basic-array*-p)` `(general-array*-p)`
- `(arrayp)` を削除
- `(equal (list t nil nil) '(t nil nil))` が nil にある問題を修正
    - `t` が真値を保持するシンボルだったが、真値を表す予約語とした

v0.2.0
======
Dec 29 2022

- embed.lsp や lsp2go.lsp などの組込み Lisp で関数やマクロを定義した
- `(defmacro)` で `(setf)` や `(set-..)` を再実装
- `<utf\*string>` を削除。`<string>` は `<utf8string>` と等価となった
- `(setf (subseq UTF8STRING START END) NEWVALUE)` をサポート
- 以下を実装
    - `(dolist)` by `(defmacro)`
    - `(dotimes)` by `(defmacro)`
    - `(lambda-macro)`
    - `(gensym)`
    - `(convert SYMBOL &lt;string&gt;)`
    - `(file-length)`
    - `(probe-file)`
    - `(backquote)`
    - `(create-array)` `(arrayp)` `(array-dimensions)` `(aref)`
    - `(abort)`
    - `(tagbody)` `(go)`
- 以下を修正
    - gmnlisp.exe: `*posix-argv*` が未定義だった
    - `(defmacro)` がレキシカルな名前空間になっていなかった
    - `(defun)`: `&rest` が評価されていなかった
    - tokenizer: \" and \\ を取り扱えていなかった
- `(block)` で第一引数で nil を与えられるようになった
- `(replica)` を (set-car) へ、`(replid)` を `(set-cdr)` へ変更
- `(quote X)` を `'X` と表示するようにした
- `(defun)` と `(defmacro)` で `:rest` を `&rest` と同様に使えるようにした

他
