[![GoDoc](https://godoc.org/github.com/hymkor/gmnlisp?status.svg)](https://godoc.org/github.com/hymkor/gmnlisp)

gmnlisp
=======

Gmnlisp is a small Lisp implementation in Go.
( Now under constructing. Experimental implementing )

![Example image](factorial.png)

```go
package main

import (
    "context"
    "fmt"
    "os"

    "github.com/hymkor/gmnlisp"
)

func main() {
    lisp := gmnlisp.New()
    lisp.DefineParameter("a", gmnlisp.Integer(1))
    lisp.DefineParameter("b", gmnlisp.Integer(2))
    value, err := lisp.Interpret(context.TODO(), "(+ a b)")
    if err != nil {
        fmt.Fprintln(os.Stderr, err.Error())
        return
    }
    value.PrintTo(os.Stdout, gmnlisp.PRINT)
    fmt.Println()
}
```

```
$ go run examples/example1.go
3
```

gmnlpp - Text preprocessor by gmnlisp
-------------------------------------

This page was generated by a preprocessor with built-in gmnlisp.
The text before proprocessed is [here](https://raw.githubusercontent.com/hymkor/gmnlisp/master/_README.md)

Support Types
-------------

- number
- integer
- float
- string
- symbol
- cons
- list
- cons
- character
- T/nil

Support functions
-----------------

- `*`
- `*standard-output*`
- `+`
- `-`
- `--get-all-symbols--`
- `/`
- `/=`
- `1+`
- `1-`
- `<`
- `<=`
- `=`
- `>`
- `>=`
- `T`
- `and`
- `append`
- `apply`
- `aref`
- `assoc`
- `atom`
- `block`
- `cadddr`
- `caddr`
- `cadr`
- `car`
- `cdddr`
- `cddr`
- `cdr`
- `close`
- `coerce`
- `command`
- `cond`
- `cons`
- `consp`
- `defmacro`
- `defparameter`
- `defun`
- `defvar`
- `detab`
- `dolist`
- `dotimes`
- `equal`
- `equalp`
- `evenp`
- `exit`
- `floatp`
- `foreach`
- `funcall`
- `function`
- `if`
- `integerp`
- `lambda`
- `last`
- `length`
- `let`
- `let*`
- `list`
- `listp`
- `load`
- `macroexpand`
- `map`
- `mapcar`
- `member`
- `minusp`
- `nil`
- `not`
- `nth`
- `nthcdr`
- `null`
- `numberp`
- `oddp`
- `open`
- `or`
- `parse-integer`
- `plusp`
- `prin1`
- `princ`
- `print`
- `progn`
- `quit`
- `quote`
- `read`
- `read-line`
- `return`
- `return-from`
- `reverse`
- `setq`
- `split-string`
- `strcase`
- `strcat`
- `stringp`
- `strlen`
- `subst`
- `substr`
- `symbolp`
- `terpri`
- `trace`
- `truncate`
- `typep`
- `while`
- `write`
- `write-line`
- `zerop`
