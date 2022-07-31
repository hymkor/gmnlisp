[![GoDoc](https://godoc.org/github.com/hymkor/gmnlisp?status.svg)](https://godoc.org/github.com/hymkor/gmnlisp)

Gommon Lisp
===========

Now under constructing. Experimental implementing

![Example image](factorial.png)

```go
(%
    (defun detab (src)
        (apply #'strcat
            (map 'list
                #'(lambda (c)
                    (if (= #\tab c)
                        "    "
                        (coerce (list c) 'string)
                    )
                )
                src
            )
        )
    )
    (let (fd count line)
        (if (setq fd (open "examples/example1.go"))
            (progn
                (setq count 0)
                (while (setq line (read-line fd))
                    (if (>= count 3)
                        (write-line (detab line)))
                    (setq count (1+ count))
                )
                (close fd)
            )
        )
    )
%)
```

```
$ go run examples/example1.go
(% (command "go" "run" "examples/example1.go") %)
```

Support functions
-----------------

(%
    (let (name)
        (foreach name (--get-all-symbols--)
            (write-line (strcat "- `" name "`"))))
%)
