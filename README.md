[![GoDoc](https://godoc.org/github.com/hymkor/gmnlisp?status.svg)](https://godoc.org/github.com/hymkor/gmnlisp)

Gommon Lisp
===========

Now under constructing.  
Experimental implementing

Example-1
---------

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

Example-2
---------

```
$ cat examples/factorical.lsp
(defun factorial (n)
  (if (<= n 1)
    1
    (* n (factorial (- n 1)))))
(factorial (parse-integer (car *posix-argv*)))
$ gmnlisp.exe examples/factorical.lsp 6
720
```

Support functions
-----------------

- \*
- \+
- \-
- /
- =
- T
- \<
- \<=
- \>
- \>=
- append
- atom
- block
- car
- cdr
- cond
- cons
- defun
- equal
- equalp
- if
- lambda
- let
- list
- nil
- prin1
- princ
- print
- progn
- quote
- return
- return-from
- setq
- terpri
- truncate
