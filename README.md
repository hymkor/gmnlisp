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
    value.PrintTo(os.Stdout)
    fmt.Println()
}
```

```
$ go run examples/example1.go
3
```

Support functions
-----------------

- `-`
- `--get-all-symbols--`
- `*`
- `*posix-argv*`
- `*standard-output*`
- `/`
- `+`
- `<`
- `<=`
- `=`
- `>`
- `>=`
- `and`
- `append`
- `atom`
- `block`
- `car`
- `cdr`
- `close`
- `cond`
- `cons`
- `defmacro`
- `defun`
- `equal`
- `equalp`
- `exit`
- `if`
- `lambda`
- `let`
- `list`
- `macroexpand`
- `nil`
- `open`
- `or`
- `parse-integer`
- `prin1`
- `princ`
- `print`
- `progn`
- `quit`
- `quote`
- `read-line`
- `return`
- `return-from`
- `setq`
- `T`
- `terpri`
- `truncate`
- `while`
- `write`
- `write-line`
