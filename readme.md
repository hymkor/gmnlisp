Gommon Lisp
===========

Now under constructing.  
Experimental implementing

- \*
- \+
- \-
- \/
- atom
- block
- car
- cdr
- cond
- cons
- defun
- equal
- lambda
- let
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
