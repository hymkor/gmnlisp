[TP Result]: https://github.com/hymkor/gmnlisp/blob/master/how-to-verify.md

- Fix: ensure APPLY raises error for improper list in final argument
- Fix APPLY to check number of arguments before validating the function argument
- Fix: recognize character names like #\Space case-insensitively
- Reimplement `simple-error`, `simple-error-format-string`, and `simple-error-format-arguments` in Go
- Create and use new function `%make-simple-error` instead of `(create <simple-error>)` at `(error)` and `(cerror)`

v0.7.10
=======
May 6 2025

- Modified the functions `div` and `mod` to ensure that the division result is the greatest integer less than or equal to the quotient when negative values are passed as arguments.
- Reserved symbols such as `nil`, `t`, `*pi*`, `*most-positive-float*`, and `*most-negative-float*` are now recognized and rejected when used improperly as identifiers in `let`, `let*`, `lambda`, `defconstant`, and `defglobal` forms. These cases now raise `<program-error>` as required.
- Introduced a new `Reserved` type to represent such reserved symbols, which are now identified during parsing.
- `(defglobal)` now raises an error if the symbol has already been defined as a constant.
- Fix: `(or)` with no arguments now correctly returns `nil` instead of raising an error
- `(defclass)` now raises an error when a special form name is used as the class name
- **Symbols: previously case-sensitive, but now equality is case-insensitive; symbols display in uppercase and preserve original case for string conversion**
- Fix `(convert nil <type>)` not behaving according to the specification

[TP Result] : OK = 11533, NG = 4878

v0.7.9
======
Feb 15, 2025

- Renamed the type `_OutputFileStream` to `outputStream`.
- The standard output and the error output now use `outputStream`.
- Fixed an issue `~&` of `(format)` inserted a new line even when the cursor was at the beginning of the line.
- Made `_WriteNode` and `outputStream` completely independent of each other.
- Made the executable include the macro `(assert-eq)` which was defined on test lisp files. It is not contained in the gmnlisp package.
- Incorporated the macro `(assert-eq)`, previously defined as `(test)` in test Lisp files, into the gmnlisp executable. Note that it is not included in the gmnlisp package.
- In interactive mode, parentheses are now colored differently for each nested level

[TP Result] : OK = 11040, NG = 5371

v0.7.8
======
Jan 16, 2025

- Fixed: `(+)` returned `nil` instead of `0` as expected.
- Fixed: `(and)` produced an error instead of returning `t`.
- Fixed: `(equal)` always returned `nil` when comparing `<input-stream>` instances
- Fixed: `(equal)` always returned `nil` when comparing `<output-stream>` instances.
- Fixed: `(get-string-output-stream)` did not reset `(create-string-output-stream)` instances as required.
- Fixed: `(case-using PREDFORM ...)` did not validate the type of PREDFORM.
- Fixed: `(set-car OBJ CONS)` and `(set-cdr OBJ CONS)` returned `CONS` instead of `OBJ`
- Fixed: `(equal)` did not function correctly when comparing instances of `<stream-error>` or `<parse-error>`.
- Fixed: `(write-byte)` returned an incorrect error instead of `<domain-error>`.
[TP Result] : OK = 11040, NG = 5371

v0.7.7
======
Dec 30, 2024

- Implement `(defconstant)`, `(gcd)`, `(lcm)`, `(preview-char)`, `(format-fresh-line)`, `(map-into)`, `(exp)`, `(sin)`, `(cos)`, `(tah)`, `(sinh)`, `(cosh)`, `(tanh)`, `(atan)`, `(abs)`, `(log)`, `*most-negative-float*` and `*most-positive-float*`
- Fix: `too many arguments` / `too few arguments` were not `DomainError`
- Fix: type errors for `+`,`-`,`*`,`div`,and `mod` were not `DomainError`
- Fix: `(lambda)` returned `<program-error>` on the case it should return `<domain-error>`
- Fix: the number of the parameters of `(eq)`, `(eql)`, `(equal)`, `(equalp)` and `(div)` could be any number. It should always 2
- readline: erase continuation prompt after submiting for copying with mouse

[TP Result] : OK = 10217, NG = 6194

v0.7.6
======
Dec 25, 2024

- `(format W)` throws `<domain-error>` when W is not io.Writer
- `(format)` supports `~nT`
- Implement `(format-tab W COLUMN)`, `(streamp)`, `(input-stream-p)`, `(output-stream-p)`, `(open-stream-p)`, `(open-io-file)`, `(with-open-io-file)`, `(with-standard-output)`, `(with-error-output)`, and `(stream-ready-p)`
- `(undefined-entity-namespace)` returns `'dynamic-variables` now for `<undefined-entity>` returned by `(dynamic)`
- Remove class-names `<_WriterNode>`, `<reader>`, `<output-file-stream>`, `<input-stream>`, `<stream-set-file-position>` and `<input-output-stream>`, and add `<stream>`
- Support `RESULT` of `(dolist (VAR INIT-FORM RESULT) FORM...)`
- Support `RESULT` of `(dotimes (VAR LIMIT RESULT) FORM...)`

[TP Result] : OK = 8642, NG = 7769

v0.7.5
======
Dec 18, 2024

- Implement `(file-position)`, `(identity)`, `(read-byte)`, `(set-file-position)`, `(write-byte)`, and `(stream-error-stream)`
- Fix: `(open-input-file)`, and `(open-output-file)`: error when two arguments were given
- When `(with-handler)` returns normally without non-local-exists, it occurs `<control-error>`(`Handler return normally`) and it can be handled with higher-level handlers
- Rename the command name of division from `(/ Z1 Z2)` to `(div Z1 Z2)` same as ISLisp
- gmnlisp.exe: set the position of the standard-output and the error-output to the top of the line for `~&` of `(format)`

[TP Result] : OK = 8214, NG = 8197

v0.7.4
======
Dec 8, 2024

- Add `NewLineOnFormat` as the character for `~%` (default: `[]byte{'\n'}`)
- Support unicode character literal `#\U3042` like CommonLisp
- Implement: `(get-universal-time)`, `(get-internal-real-time)`, `(get-internal-run-time)` and `(internal-time-units-per-secon)`

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

- Fix: `(aref)`: the number and range of parameters were not checked
- Fix: `(create-string)`: the range of parameter was not checked
- Fix: `(string-append)` was `nil`, but should be `""`
- Implement `<end-of-stream>`
- Fix: `(format)` paniced when base number is less than 2 or greater than 36.
- Prevent signal handlers from going into infinite loop

The ISLisp verification program now runs without crashing until the final test.
The current score is `TP Result: OK = 7889, NG = 8522`

v0.7.1
======
Jul 23, 2024

- Fix: `(equal USER-DEFINED-CLASS-OBJECT...)` was always false.
- Implement `(assure)`, `(the)`, `(max)`, `(min)`, `(eval)`, `(arithmetic-error-operands)`, `(arithmetic-error-operation)`, `<program-error>`, and `arity-error`
- On any built-in-class CLASS, both `(subclassp CLASS <built-in-class>)` and `(subclassp CLASS <object>)` are `t`
- Macros within functions are now expanded when the function is defined (previously it was always done when the function was called).
- Implement `(expand-defun)` which displays the definition of the function
- Fix the problem each element of array literal should not be evaluated, but it was
- Fix crashed when print cons whose car-part is nil
- Fix the result of `(for)` was sometimes nil
- Remove Eval() from requirements of Node interface, and let user's program use the receiver itself instead if Eval() does not exist
- Remove PrintTo() and GoString() from the requirements of Node interface, and let user's program call String() if they do not exist
- Fix: not handled where quote(`'`) occurs immediately before unquote(`,`)
- Enable to call `((lambda ...) ...)`
- Fix: the result to evalute `(1 2)` was `<domain-error>`, now it is `<undefined-function>`
- Raise `<error>` when the parameters of `(lambda)` are duplicated now.
- Fix: `(instancep (create <domain-error>) <program-error>` was false 
- The word starting with `&` is treated same as `:`
- Fix: function defined at `(flet)` could call itself recursively
- `(defun)`,`(defgeneric)` can not re-define the special operator like `if`
- Fix: `(return-from nil ...)` failes
- `(return-from NOT-EXIST-BLOCK)` raises `<control-error>`
- `(throw NOT-EXIST-TAG)` raises `<control-error>`
- `(go)` raises `<controle-error>`
- When `(go)`, `(throw)` or `(return-from)` is called on CLEANUP-FORM of `(unwind-protect FORM (go) ..)`, raise `<control-error>`
- Implement `<strage-exhausted>`
- `(create <array>)` without arugments raises an error now
- Fix: `(create-array ()...)` crashed
- `(read)` can throw `<parse-error>` now
- Implement BigInt minimally to read integer overflow with int65
- Implement `<number>` as the base class for `<integer>` and `<float>`
- Fix: `(create-array)` crashed when one argument

v0.7.0
======
Jun 27, 2024

- Implement the type function reference
    - `(lambda)`, `(function)` and `#'` return not a function itself, but a reference to a function now
    - `(funcall)`, `(map*)`, `(labels)`, and `(flets)` require not a function but a reference, and raise an error when a function itself is given
- Split the namespace for functions and that of variables
- Implement the error class `<undefined-function>`, methods: `(undefined-entity-name)`,`(undefined-entity-namespace)`
- `(function)` returns error when a macro,special form is given as a parameter (On ISO, the consequence is undefined)

v0.6.0
======
Jun 25, 2024

### Fixed bugs

- Fix: KEY on `(case KEYFORM ((KEY*) FORM*)*)` was evaluated though it should not
- Fix: `(apply)` would double evalute the last argument

### Generic functions

- Implement `(defgeneric)`, `(defmethod)`, and `(generic-function-p)`

### Objects

- Implement `(class-of)`, `(instancep)`, `(class)`, `(subclassp)`,
    and `(initialize-object`),
- `(create)` can create the instance of not only user-defined class,
    but also embeded-types
- `(defclass)`: support `:boundp` for slot-definition

### Condition system

- A conditiones can be implemented with a class now
- Implement `(with-handler)`, `(signal-condition)`, `(continue-condition)`,
  `(error)`, `(cerror)`, `(report-condition)`, and `<simple-error>`

Now, all of errors have not been changed to condition object

### Properties operations

- Implement `(property)`, `(set-property)`, and `(remove-property)`

### Miscellaneous

- Implement `(sqrt)` and `(with-standard-input)`
- `(defconstant)` is defined as alias of `(defglobal)` temporally

v0.5.0
======
Jun 14, 2024

- Support exponential representation of floating point real numbers
- Add integer formats: `#b..`, `#o..`, and `#x..`
- Implement `(ignore-errors FORMS...)`
- Implement `(defclass)` and `(create)`
- gmnlisp.exe: go-multiline-ny v0.12.1 → v0.15.0 - improving history

v0.4.1
======
Oct 01, 2023

- gmnlisp.exe: Use reverse(ESC[7m) and underline(ESC[4m) for SKK conversion
- gmnlisp.exe: Fix: SKK failed to start when user-jisyo did not exist

v0.4.0
======
Sep 30, 2023

- gmnlisp.exe: support multi-line editing by go-multiline-ny
- gmnlisp.exe: support Japanese input method editor SKK by go-readline-skk  
    To use SKK,
    - (Windows): `set "GOREADLINESKK=(system-jisyo-paths..);user=(user-jisyo-path)"`  
        for example `set "GOREADLINESKK=~/Share/Etc/SKK-JISYO.*;user=~/.go-skk-jisyo"`
    - (Linux): `export "GOREADLINESKK=(system-jisyo-paths..):user=(user-jisyo-path)"`

v0.3.1
======
Sep 11, 2023

- Add tool type and functions for golang applications
    - type `Dynamics` and its methods
    - `(*World) NewDynamics` and `(*World) Dynamic`
    - test code with `(dynamic...)`

v0.3.0
======
Jul 29, 2023

- Support the symbol whose name is enclosed by vertical-bars (Specification of ISLisp)
- Changed display format of stack trace
- Implement `(*World) Range(Symbol,Node)` to provide an iterator of each variable.
- Implement `(gmn:dump-session)` to print all variables' names and values.
- Support following cases of tail recursion optimization:
    - `(defun X () .. (X) )`
    - `(defun X () .. (progn (X)) )`
    - `(defun X () .. (if .. (X) (X)))`
    - `(defun X () .. (let (..) .. (X)))`
    - `(defun X () .. (let* (..) .. (X)))`
    - `(defun X () .. (cond ... (t (X))))`
- Fix: (format): the sequence "~X" (X is an upper case letter) did not work
- Remove the sub packages: "pkg/auto" and "pkg/common"

V0.2.1
======
Jan 29, 2023

- Support (format FD "~N%")
- Rename HasValue to IsSome
- Rename IsNull to IsNone
- Implement
    - (char&lt;) (char&gt;) (char=) (char&lt;=) (char&gt;=) (char/=)
    - (characterp)
    - (create-list)
    - (char-index)
    - (basic-array-p) (basic-array\*-p) (general-array\*-p)
- Remove (arrayp)
- Fix: the problem (equal (list t nil nil) '(t nil nil)) was nil
    - `t` was the symbol containing True. `t` is now the reserved word meaning True.

v0.2.0
======
Dec 29, 2022

- Some functions and macros are defined by embeded Lisp (embed.lsp and lsp2go.lsp)
- Re-implement (setf) and (set-..) by (defmacro)
- Remove &lt;utf\*string&gt;. &lt;string&gt; is same as &lt;utf8string&gt;
- Support (setf (subseq UTF8STRING START END) NEWVALUE).
- Implement
    - (dolist) by (defmacro)
    - (dotimes) by (defmacro)
    - (lambda-macro)
    - (gensym)
    - (convert SYMBOL &lt;string&gt;)
    - (file-length)
    - (probe-file)
    - (backquote)
    - (create-array) (arrayp) (array-dimensions) (aref)
    - (abort)
    - (tagbody) (go)
- Fix
    - gmnlisp.exe: \*posix-argv\* was not be defined
    - (defmacro) did not support lexical namespace
    - (defun): &rest were not evaluted.
    - tokenizer: could not treat \" and \\
- (block) now accepts nil as the first parameter
- (replica) -&gt; (set-car) and (replid) -&gt; (set-cdr)
- (quote X) is displayed as `'X`
- (defun) and (defmacro) can now use :rest same as &amp;rest

v0.1.2
======
Oct 22, 2022

- Fix: (defmacro)'s bugs and support &rest of (defmacro) and ,@
- Remove (macroexpand)

v0.1.1
======
Oct 16, 2022

Fix: gmnlpp: forgot replacing `\` to `\\`

v0.1.0
======
Oct 15, 2022

- The first release
