Gommon Lisp
===========

Now under constructing.  
Experimental implementing

- \+
- \-
- atom
- car
- cdr
- cond
- cons
- defun
- eq
- lambda
- let
- print
- progn
- quote
- setq

```
$ type seq.lsp
(defun seq (n)
  (print n)
        (cond
                ((eq n 0) (print 0))
                (T (seq (- n 1)))
        )
)
(seq 10)

$ gmnlisp.exe seq.lsp
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
0
()
```
