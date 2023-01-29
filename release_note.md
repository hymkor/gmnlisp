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
