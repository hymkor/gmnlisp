[TP Result]: https://github.com/hymkor/gmnlisp/blob/master/how-to-verify.md

- `(read)` and parser convert `\r\n` to `\n` in string literals.
- Fixed a crash in `(subseq)` that occurred with certain argument combinations
- Fixed: Numeric strings starting with a plus sign were mistakenly recognized as symbols
- Fixed: Numeric strings representing large integers were incorrectly treated as floating-point numbers
- Fixed: Comparison between references to functions now behaves as expected
- Implemented class definitions related to arrays: `<basic-array>`, `<basic-array*>`, `<general-array*>`, `<basic-vector>`, and `<general-vector>`
- Implemented class definitions related to generic functions: `<generic-function>` and `<standard-generic-function>`
- Fixed: Non-class ordinary objects were incorrectly treated as subclasses of `<built-in-class>`
- Defined the class `<serious-condition>`
- Fixed: `<arithmetic-error>` was mistakenly referred to as `<arithmetic-error-class>`
- Fixed: `<division-by-zero>`, `<floating-point-overflow>` and `<floating-point-underflow>` did not inherit from `<object>`, `<serious-condition>`, `<error>`, and `<arithmetic-error>` as expected
- Fixed: Signed binary, octal, and hexadecimal numbers were not recognized as integers
- Fixed: Numeric literals ending with a decimal point did not cause an error
- Fixed: `(-)`, `(max)`, and `(min)` now raise appropriate errors when called with non-numeric arguments or with zero arguments
- Implemented `(atan2)`
- Changed `(round)` to use Go's `math.RoundToEven` instead of `math.Round` for calculation
- Fixed: `(create-list)` now raises an error if the first argument is not a non-negative integer or if the number of arguments is not 1 or 2
- Standardized the number of elements required to trigger `<storage-exhausted>` to 123,456,789 as a shared constant
- Added validation of the second argument (element-class) for `(file-length)`, `(open-input-file)`, `(open-output-file)`, and `(open-io-file)`
- Added the `<invalid>` class for compatibility with ISLisp conformance test systems
- Improved `(*)` to detect floating-point overflow and underflow as `<floating-point-overflow>` or `<floating-point-underflow>`
- Modified `(set-file-position)` to return `<domain-error>` when given a negative offset
- Extended `(convert)` to support additional types, including `<general-vector>`
- Fixed: Missing parameter count checks in `<`, `<=`, `>`, and `>=`
- Fixed: `(finish-output)` no longer raises `<domain-error>` when given an unbuffered `<stream>` such as `(standard-output)`; it now performs no operation in such cases

v0.7.18
=======
Jul 14, 2025

- Stack traces on error now show not only function names but also their arguments.
- `(elt)` now raises an index out of range error as a `<domain-error>`.
- Extended `<domain-error>` with a `Reason` field to represent non-type errors more accurately.
- Even when `set-elt` is applied to an immutable string, it now first checks whether the new value is a `<character>`.
- Fixed: `(go)` did not check the number of arguments.
- Corrected the class name of `<number>` from `number` to `<number>`.
- `(defclass)` no longer overwrites existing built-in classes.
- `(create-array)` now checks whether its first argument is a list of non-negative integers.
- `(format)`: `~B`, `~X`, `~O`, and `~D` now raise `<domain-error>` if given non-integer arguments.
- `(format)`: `~G` now raises `<domain-error>` if given a non-real argument.
- `(format)`: Added support for `~C` and `~R`.
- `(with-standard-input)` now checks whether its argument is a stream.
- Fixed a bug where an empty array was displayed as `#)` instead of `#()`.
- Added the `-p` option to the executable to print the result of the last evaluated expression.
- Disabled output buffering by default for standard output and standard error.
- Changed `(subseq)` to return a `<domain-error>` when given out-of-range arguments.
- `(subseq)` now supports `<general-array>`

[TP Result] : PASS = 14266, FAIL = 2145

v0.7.17
=======
Jul 7, 2025

- Implemented `(condition-continuable)`
- Exported the internal type `_WriteNode` as `WriterStream`
    - Also exported its constructor `NewWriterStream`
- Exported the internal type `_BuiltInClass` as `BuiltInClass`
    - Also exported its constructors `NewBuiltInClass` and `NewAbstractClass`
- Exported the class object `objectClass` as `ObjectClass`
- Exported the class object `builtInClass` as `BuiltInClassObject`
- Added a `RawWriter` method to `<output-stream>`-related objects to expose the underlying `io.Writer`
- Consolidated previously separate types implementing both `Node` and `error` interfaces into a single type `Condition`
- Introduced the `Continuable` interface to represent continuable states, now used by `(signal-condition)` and `<simple-error>`
- Fixed an issue where `(return-from)` could be used even when the tag was not visible in the **lexical scope** (like `catch`/`throw` tags), and ensured conformance to the standard by signaling a `<control-error>` in such cases

[TP Result] : PASS = 13711, FAIL = 2700

v0.7.16
=======
Jun 24, 2025

- When running the gmnlisp executable, `*executable-name*` is set to its path. (Compatible with [lispect](https://github.com/hymkor/lispect) )
- Although a class name like `<xxxxx>` used to represent a class object itself, it is now just a symbol, and `class <xxxxx>` is required to obtain the class object.
- Fixed a bug where the value of the cleanup-form was incorrectly used as the result of `(unwind-protect)`.
- Fixed a bug where `(set-property)` did not return the assigned value.
- Fixed a bug where `(remove-property)` did not return the removed value.
- `(mapc)`,`(mapcan)`,`(marpcar)`,`(mapcon)`,`(mapl)`, `(maplist)`, `(reverse)` and `(append)` now check whether the parameters are lists.
- `(continue-condition)` now checks the number of arguments and verifies that the first argument is a condition.
- When running the gmnlisp executable, `*temp-dir*` is set to the default directory to use for temporary files.
- Experimentally implemented symbol name completion in interactive mode.

[TP Result] : PASS = 13687, FAIL = 2724

v0.7.15
=======
Jun 16, 2025

- Fix: `(set-elt NEWVALUE SEQ Z)` returned SEQ though it should return NEWVALUE
- Fix: `(case 1 (1))` should raise error, but returned nil
- Fix: `(*Pair) Set` always retured an error
- Fix: Where the same variable declared twice in a `(let*)` expression was treated as the same variable.
- Fix: Multiple declarations of the same variable in `let` or `for` were not reported as errors.
- Fix: numeric or character tags in `(catch)` were not treated as errors.
- `(case-using)` now returns nil when action section is empty
- Fix: `(case NIL NIL)` and `(case NIL NIL NIL)` raise `<domain-error>` instead of `<program-error>`
- Changed REPL output format to print values as S-expressions (~S-style) instead of raw strings (~A-style).
- Add platform-specific `*dev-null*` constant to executable
    (evaluates to `"NUL"` on Windows and `"/dev/null"` on other platforms)
- Fixed incorrect behavior where `(equalp)` acted like `(=)`
- Fixed outdated custom test code to align with the latest specification  
  (Note: No issue in the main implementation; tested using the ISLisp verification system)  
  - `(property)` was expected to signal an error when the property was missing, but it should return the third argument or `nil`
  - `(=)` was incorrectly used to compare case-insensitive strings
  - `(=)` was used with more than two arguments, which is not allowed
- Fixed a bug in `case` and `case-using` where forms following a `t` clause were not properly rejected as an error.
- `index-ouf-of-range` is now one of `<program-error>`
- Renamed `(set-aref)` to `(set-garef)` since its behavior was effectively equivalent to `(set-garef)`.
- Fixed an issue where the symbol’s ID number was printed instead of its name when displaying results in interactive mode.
- Implement `(garef)`, `(basic-vector-p)`, `(general-vector-p)`, `(isqrt)`, `(finish-output)`, `(float)`, `(expt)`, `(reciprocal)` and `(quotient)`
- Move subpackages from ./pkg/ to ./
- Moved `(eval)` and `(load)` from the core package to a subpackage: `"github.com/hymkor/gmnlisp/eval"`. Use `import _ "github.com/hymkor/gmnlisp/eval"` to enable them.

[TP Result] : PASS = 13175, FAIL = 3236

v0.7.14
=======
Jun 9, 2025

- Fixed test failures related to `(cond)`
- Fixed test failures related to `(generic-function-p)`
- In interactive mode, print the result value even when it is nil.
- `(macroexpand ARG)` now returns ARG when ARG is not a macro form.
- When the first argument is a macro, `(setf)` expands it before evaluting.
- `(macroexpand ARG)` now recursively expands ARG until no macro form remains.

[TP Result] : PASS = 12127, FAIL = 4284

v0.7.13
=======
May 26, 2025

- Add parameter validations: `(=)`, `(/=)`, `(format-integer)`, `(format-float)` and `(close)`
- `(dynamic)` now checks the number of arguments
- `(setf (dynamic X) V)` now checks whether X exists and returns V
- Macros now checks the number of arguments
- Implement zero-dimension literal
- Implement the strict mode and `-strict` option
- When the strict mode, t and nil are not treated as streams
- When the strict mode, `dynamic-let`, `let` and `let*` raise the error if each parameter is a single symbol rather than list
- When the strict mode, `setq` raises an error when the number of parameters it not two
- `(dynamic :A)` now raises the undefined entity error rather than the domain error

[TP Result] : PASS = 12090, FAIL = 4321

v0.7.12
=======
May 19, 2025

- Fix: `(equal)` did not recursively compare vector elements
- Implement `(create-vector)`
- `(property)`, `(set-property)` and `(remove-property)` now validate that arguments are symbols
- `(property)` no longer raises an error when the symbol or property name is not found
- `(property)` now uses the last argument according to the specification
- Add parameter validations `(instancep)`
- Add parameter validations `(subclassp)`

[TP Result] : PASS = 11918, FAIL = 4493

v0.7.11
=======
May 12, 2025

- Fix: ensure APPLY raises error for improper list in final argument
- Fix APPLY to check number of arguments before validating the function argument
- Fix: recognize character names like #\Space case-insensitively
- Reimplement `simple-error`, `simple-error-format-string`, and `simple-error-format-arguments` in Go
- Create and use new function `%make-simple-error` instead of `(create <simple-error>)` at `(error)` and `(cerror)`
- `error` and `cerror` now validates if arguments are strings
- `(signal-conditions)` now checks whether the argument inherits from `<error>`
- Change `for` macro to return the result of `(progn . result*)` instead of using `(elt result* 1)`.
- `for` now checks whether the length of iteration-spec is two or three
- Add a new system variable `*argv*` with the same contents as `*posix-argv*`.

[TP Result] : PASS = 11677, FAIL = 4734

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

[TP Result] : PASS = 11533, FAIL = 4878

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

[TP Result] : PASS = 11040, FAIL = 5371

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
[TP Result] : PASS = 11040, FAIL = 5371

v0.7.7
======
Dec 30, 2024

- Implement `(defconstant)`, `(gcd)`, `(lcm)`, `(preview-char)`, `(format-fresh-line)`, `(map-into)`, `(exp)`, `(sin)`, `(cos)`, `(tah)`, `(sinh)`, `(cosh)`, `(tanh)`, `(atan)`, `(abs)`, `(log)`, `*most-negative-float*` and `*most-positive-float*`
- Fix: `too many arguments` / `too few arguments` were not `DomainError`
- Fix: type errors for `+`,`-`,`*`,`div`,and `mod` were not `DomainError`
- Fix: `(lambda)` returned `<program-error>` on the case it should return `<domain-error>`
- Fix: the number of the parameters of `(eq)`, `(eql)`, `(equal)`, `(equalp)` and `(div)` could be any number. It should always 2
- readline: erase continuation prompt after submiting for copying with mouse

[TP Result] : PASS = 10217, FAIL = 6194

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

[TP Result] : PASS = 8642, FAIL = 7769

v0.7.5
======
Dec 18, 2024

- Implement `(file-position)`, `(identity)`, `(read-byte)`, `(set-file-position)`, `(write-byte)`, and `(stream-error-stream)`
- Fix: `(open-input-file)`, and `(open-output-file)`: error when two arguments were given
- When `(with-handler)` returns normally without non-local-exists, it occurs `<control-error>`(`Handler return normally`) and it can be handled with higher-level handlers
- Rename the command name of division from `(/ Z1 Z2)` to `(div Z1 Z2)` same as ISLisp
- gmnlisp.exe: set the position of the standard-output and the error-output to the top of the line for `~&` of `(format)`

[TP Result] : PASS = 8214, FAIL = 8197

v0.7.4
======
Dec 8, 2024

- Add `NewLineOnFormat` as the character for `~%` (default: `[]byte{'\n'}`)
- Support unicode character literal `#\U3042` like CommonLisp
- Implement: `(get-universal-time)`, `(get-internal-real-time)`, `(get-internal-run-time)` and `(internal-time-units-per-secon)`

[TP Result] : PASS = 7903, FAIL = 8508

v0.7.3
======
Nov 29, 2024

- Fix: `NG: (defun foo) -> #<Error> <error> [#<Error> <program-error>]`
- Fix: `NG: (defun t nil) -> #<Error> <domain-error> [t]`
- Fix: `NG: (defun nil nil) -> nil`
- Fix: `NG: (create (class <standard-class>)) -> panic: runtime error`

[TP Result] : PASS = 7891, FAIL = 8520

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
The current score is `TP Result: PASS = 7889, FAIL = 8522`

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
