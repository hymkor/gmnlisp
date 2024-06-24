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
