[TP Result]: https://github.com/hymkor/gmnlisp/blob/master/how-to-verify.md

- 最後の引数が適切なリストでない場合 APPLY はエラーを起こすようにした
- 「関数」引数の検査よりも前に引数チェックをするよう APPLY を修正した。
- #\Space のような文字の名前の英大文字小文字の区別をしないよう修正した。

v0.7.10
=======
May 6 2025

- マイナスの値を与えられても、割り算の結果が商以下の最大の整数になるよう、関数 `div` と `mod` を修正した
- `nil`、`t`、`*pi*`、`*most-positive-float*`、`*most-negative-float*` などの予約語を、`let`, `let*`, `lambda`、`defconstant`、`defglobal` フォームで識別子として使った場合に `<program-error>` を発生させるようにした。
- 予約語を表現する新しい型 `Reserved` を導入し、構文解析の段階で認識するようにした。
- シンボルが定数として定義済みの時、`(defglobal)` はエラーになるようにした。
- 引数のない `(or)` で、エラーとせずに、正しく `nil` を返すよう修正した。
- `(defclass)` で、特殊形式名がクラス名として使われた時にエラーを起こすようにした。
- **これまではシンボルは英大文字・小文字を区別していたが、比較において区別をしないようにした。表示は英大文字とするようにした。シンボルから文字列に戻す時は最初に作られた英大文字・小文字を復元するようにした**
- `(convert nil 各種型)` が仕様通りとなっていなかった点を修正した

[TP Result] : OK = 11533, NG = 4878

v0.7.9
======
Feb 15, 2025

- `_OutputFileStream` を `outputStream` に改名
- 標準出力・標準エラー出力は `outputStream` で使うよう修正
- `(format)` の `~&` で印刷位置が行頭の時でも改行する場合があった点を修正
- `_WriteNode` と `outputStream` は完全に独立した型とした
- テスト用 Lisp ファイルで定義されていた `(test)` マクロを、`(assert-eq)` という名前でgmnlisp の実行ファイルに組み込んだ。なお、gmnlisp パッケージには含んでいない
- インタラクティブモードで、括弧はネストレベルごとに違う色付けをするようにした

[TP Result] : OK = 11040, NG = 5371

v0.7.8
======
Jan 16, 2025

- `(+)` が `0` ではなく、`nil` になっていた不具合を修正
- `(and)` が `t` ではなく、エラーを出力していた不具合を修正
- `<input-stream>` のクラスインスタンスどうしの`(equal)` が常に false になっていた不具合を修正
- `<output-stream>` のクラスインスタンスどうしの `(equal)` が常に false になっていた不具合を修正
- `(get-string-output-stream)` で、`(create-string-output-stream)` のインスタンスをクリアしていなかった
- `(case-using PREDFORM ...)` で PREDFORM の型をチェックしていなかった
- `(set-car NEWOBJ CONS)`, `(set-cdr NEWOBJ CONS)` の戻り値が NEWOBJ ではなく CONS になっていた
- `(equal)` が `<stream-error>` や `<parse-error>` のインスタンス間で正しく動作していなかった
- `(write-byte)` が `<domain-error>` を返していなかった

[TP Result] : OK = 11040, NG = 5371

v0.7.7
======
Dec 30, 2024

- `(defconstant)`, `(gcd)`, `(lcm)`, `(preview-char)`, `(format-fresh-line)`, `(map-into)`, `(exp)`, `(sin)`, `(cos)`, `(tah)`, `(sinh)`, `(cosh)`, `(tanh)`, `(atan)`, `(abs)`, `(log)`, `*most-positive-float*`, `*most-negative-float*` を実装
- `too many arguments` / `too few arguments` が `DomainError` になっていなかった不具合を修正
- 四則演算の型エラーが DomainError になっていなかった不具合を修正
- `(lambda)` で `<domain-error>` を返すべきケースで、`<program-error>` を返していた点を修正
- `(eq)`,`(eql)`,`(equal)`,`(equalp)`,`(div)` のパラメータの個数は2個固定なのに、任意個数が可能だった点を修正
- マウス操作によるコピー向けに入力終結後に継続プロンプトを消去するようにした。

[TP Result] : OK = 10217, NG = 6194

v0.7.6
======
Dec 25, 2024

- `(format W)` は W が Writer でない時に `<domain-error>` を発生するようにした
- `(format)` で `~nT` をサポート
- `(format-tab W COLUMN)`,`(streamp)`,`(input-stream-p)`,`(output-stream-p)`,`(open-stream-p)`, `(open-io-file)`, `(with-open-io-file)`, `(with-standard-output)`, `(with-error-output)`, `(stream-ready-p)` を実装
- `(dynamic)` が返す `<undefined-entity>` に対する `(undefined-entity-namespace)` は `'dynamic-variable` を返すようにした
- クラス名 `<_WriterNode>`, `<reader>`, `<output-file-stream>`, `<input-stream>`, `<input-output-stream>`, `<stream-file-position>`, `<stream-set-file-position>` を廃止し、`<stream>` を追加
- `(dolist (VAR INIT-FORM RESULT) FORM...)` の RESULT をサポート
- `(dotimes (VAR LIMIT RESULT) FORM...)` の RESULT をサポート

[TP Result] : OK = 8642, NG = 7769

v0.7.5
======
Dec 18, 2024

- `(file-position)`, `(identity)`, `(read-byte)`, `(set-file-position)`, `(write-byte)`, `(stream-error-stream)` を実装
- `(open-input-file)`, `(open-output-file)` で引数が二つの時にエラーになる問題を修正
- `(with-handler)` のハンドラーが非局所脱出せずに普通に終了した時、`Handler return normally` という `<control-error>` を発生して、上位のハンドラーで処理できるようにした。
- `(/ Z1 Z2)` だった除算を `(div Z1 Z2)` にリネーム (ISLisp 対応)
- gmnlisp.exe: コマンド入力の直後に、`(format)` の `~&` のためにカウントしている標準出力と標準エラー出力の桁位置を行頭扱いにセットするようにした

[TP Result] : OK = 8214, NG = 8197

v0.7.4
======
Dec 8, 2024

- `~%` に用いる文字として `NewLineOnFormat` を追加(デフォルトは `[]byte{'\n'}`)
- CommonLisp のような Unicode 文字リテラル(`#\U3042`) をサポート
- `(get-universal-time)`, `(get-internal-real-time)`, `(get-internal-run-time)`, `(internal-time-units-per-secon)` を実装

[TP Result] : OK = 7903, NG = 8508

v0.7.3
======
Nov 29, 2024

- Fix: `NG: (defun foo) -> #<Error> <error> [#<Error> <program-error>]`
- Fix: `NG: (defun t nil) -> #<Error> <domain-error> [t]`
- Fix: `NG: (defun nil nil) -> nil`
- Fix: `NG: (create (class <standard-class>)) -> panic: runtime error`

[TP Result] : OK = 7891, NG = 8520

v0.7.2
======
Jul 29, 2024

- `(aref)`: パラメータの個数チェック・範囲チェックをしていなかった不具合を修正
- `(create-string)`: パラメータの範囲チェック漏れを修正
- `(string-append)` が `""` ではなく `nil` になっていた
- `<end-of-stream>` を実装
- `(format)` で基数が2未満になったり36を超過した時に panic にならないようにした
- シグナルハンドラーが無限ループしないようにした

ISLisp の検証プログラムが最後のテストまで落ちることなく走るようになった。
現在のスコアは `TP Result: OK = 7889, NG = 8522`

v0.7.1
======
Jul 23, 2024

- `(equal)` でユーザ定義クラスのオジェクトの比較が常に不一致になる問題を修正
- `(assure)`, `(the)`, `(max)`, `(min)`, `(eval)`, `(arithmetic-error-operation)`, `(arithmetic-error-operands)`, `<program-error>`, `arity-error` を実装
- 任意の組み込みクラス CLASS において `(subclassp CLASS <built-in-class>)` , `(subclassp CLASS <object>)` がともに `t` になるようにした
- 関数内のマクロは関数定義時に展開するようにした(今まで常に呼び出し時に行っていた)
- `(defun)` 定義内容を表示する `(expand-defun)` を実装
- 配列リテラルを評価した時に各要素全てを再評価すべきではなかった点を修正
- cons を表示する時、car 成分が nil だとクラッシュする不具合を修正
- `(for)` の結果が nil になってしまう場合がある不具合を修正
- Node interface の要件から Eval() を外し、存在しなければ利用側でレシーバーそのものを使うようにさせた
- Node interface の要件から PrintTo(), GoString() を外し、存在しなければ利用側で String() を使わせるようにした
- `'` (quote) の直後に `,` (unquote) が来るケースをうまく読み込めない不具合を修正
- `((lambda ...) )` という呼び出しを出来るようにした
- `(1 2)` の評価結果が `<domain-error>` だったのを `<undefined-function>` に修正した
- lambda のパラメーター名が重複していたら `<error>` を発生させるようにした
- `(instancep (create <domain-error>) <program-error>` が false になっていたのを修正
- `&` で始まる単語は `:` と同様に扱うようにした
- `(flet)` の中の関数が自分自身を再帰呼び出しできてしまう不具合を修正
- `(defun)`,`(defgeneric)`  で if などの特殊演算子を上書きできないようにした
- `(return-from nil ...)` がエラーになってしまう不具合を修正
- 存在しないblock名に return-from しようとした時、`<control-error>` にするようにした
- 存在しないtag名にthrow しようとした時、`<control-error>` にするようにした
- 存在しないtag名に go しようとした時、`<controle-error>` にするようにした
- `(unwind-protect)` の CLEANUP-FORM で `(go)` などを使おうとしたら、`<control-error>` にするようにした。
- `<storage-exhausted>` を実装
- 引数なしの `(create <array>)` をエラーとするようにした
- `(create-array ()...)` がクラッシュしてしまう不具合を修正
- `(read)` で `<parse-error>`  を投げるようにした
- int64 を越える整数を読み取れるように BigInt 型を必要最小限に実装した
- `<integer>`と`float`のベースクラスとなる`<number>` を実装
- `(create-array)` の引数が1個の時にクラッシュする不具合を修正

v0.7.0
======
Jun 27, 2024

- 関数への参照型を実装した
    - `(lambda)`, `(function)`, `#'` は関数それ自体ではなく、関数への参照を返すようにした
    - `(funcall)`, `(map*)`, `(labels)`, `(flets)` は関数ではなく参照を要求し、関数自体が与えられた時はエラーを起すようにした。
- 関数の名前空間と変数の名前空間を分離した
- エラー型: `<undefined-function>`, メソッド: `(undefined-entity-name)`,`(undefined-entity-namespace)` を実装
- `(function)` ではマクロ・特殊形式・定義形式の場合はエラーとした(ISO規格では結果未定義)

v0.6.0
======
Jun 25, 2024

### 不具合修正

- `(case KEYFORM ((KEY*) FORM*)...` で KEY* は評価されるべきではないのに、評価されていた不具合を修正
- `(apply)` が最後の引数を二重に評価していた不具合を修正

### 包括関数対応

- `(defgeneric)`, `(defmethod)`, `(generic-function-p)` を実装

### クラス関連機能

- `(class-of)`, `(instancep)`, `(class)`, `(subclassp)`, `(initialize-object)` を実装
- `(create)` でユーザクラスだけでなく、システムクラスのインスタンスを作れるようにした
- `(defclass)` のスロット定義の `:boundp` をサポート

### 例外処理機能

- 例外状態をクラスで実装できるようにした。
- `(with-handler)`, `(signal-condition)`, `(continue-condition)`, `(error)`,
    `(cerror)`, `(report-condition)`, `<simple-error>` を実装

今のところ、既存のエラー処理はまだ全て Condition オブジェクト化できていません

### プロパティ操作

- `(property)`, `(set-property)`, `(remove-property)` を実装

### その他

- `(sqrt)`, `(with-standard-input)` を実装
- `(defconstant)` を実装。ただし、現状は `(defglobal)` の別名

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
