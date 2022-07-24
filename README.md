[![GoDoc](https://godoc.org/github.com/hymkor/gmnlisp?status.svg)](https://godoc.org/github.com/hymkor/gmnlisp)

Gommon Lisp
===========

Now under constructing. Experimental implementing

![Example image](factorial.png)

```go
package main

import (
    "fmt"
    "os"

    "github.com/hymkor/gmnlisp"
)

func main() {
    lisp := gmnlisp.New()
    lisp.Set("a", gmnlisp.Integer(1))
    lisp.Set("b", gmnlisp.Integer(2))
    value, err := lisp.Interpret("(+ a b)")
    if err != nil {
        fmt.Fprintln(os.Stderr, err.Error())
        return
    }
    value.PrintTo(os.Stdout,gmnlisp.PRINT)
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
- `<`
- `<=`
- `=`
- `>`
- `>=`
- `T`
- `and`
- `append`
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
- `defmacro`
- `defun`
- `detab`
- `equal`
- `equalp`
- `exit`
- `foreach`
- `funcall`
- `function`
- `if`
- `lambda`
- `let`
- `line`
- `list`
- `listp`
- `load`
- `macroexpand`
- `mapcar`
- `member`
- `name`
- `nil`
- `not`
- `open`
- `or`
- `parse-integer`
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
- `setq`
- `strcase`
- `strcat`
- `strlen`
- `substr`
- `terpri`
- `trace`
- `truncate`
- `while`
- `write`
- `write-line`
