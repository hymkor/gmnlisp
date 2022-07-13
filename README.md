[![GoDoc](https://godoc.org/github.com/hymkor/gmnlisp?status.svg)](https://godoc.org/github.com/hymkor/gmnlisp)

Gommon Lisp
===========

Now under constructing.  
Experimental implementing

Example
-------

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
- atom
- block
- car
- cdr
- cond
- cons
- defun
- equal
- equalp
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

```
$ cat t/seq.lsp
(defun seq (n)
  (print n)
  (cond
    ((equal n 0)
      (return-from seq nil))
    (T
      (seq (- n 1)))
  )
)
(seq 10)
$ gmnlisp.exe t/seq.lsp
10
9
8
7
6
5
4
3
2
1
0
```
