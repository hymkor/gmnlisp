[![GoDoc](https://godoc.org/github.com/hymkor/gmnlisp?status.svg)](https://godoc.org/github.com/hymkor/gmnlisp)

Gommon Lisp
===========

Now under constructing. Experimental implementing

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
- `mapcar`
- `member`
- `minusp`
- `name`
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
