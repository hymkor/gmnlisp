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
The text before proprocessed is [here](https://github.com/hymkor/gmnlisp/blob/master/_README.md)

Support Types
-------------

integer , float , string(UTF8 or UTF32) , symbol , cons , list , character , t/nil

Support functions
-----------------

#### Output functions

- (write STRING [:stream STREAM])
- (write-line STRING [STREAM])
- (print OBJ)
- (princ OBJ)
- (prin1 OBJ)
- (terpri [STREAM])
- (format {t|nil|STREAM} FORMAT [ARGS..])

#### float

- (truncate REAL)
- (parse-integer STRING)

#### string

- (strcase STRING)
- (concatenate 'string {STRING1...})

#### List and Sequence

- (quote {LIST|ATOM}...)
- '({LIST|ATOM}...)
- (list {LIST|ATOM}...)
- (car LIST)
- (cdr LIST)
- (cadr LIST)
- (caddr LIST)
- (cadddr LIST)
- (cddr LIST)
- (cdddr LIST)
- (first LIST)
- (second LIST)
- (third LIST)
- (rest LIST)
- (nth INDEX LIST)
- (nthcdr INDX LIST)
- (length LIST)
- (last LIST)
- (reverse LIST)
- (append LIST...)
- (find ATOM LIST [:test #'TESTFUNC])
- (member ATOM LIST [:test #'TESTFUNC])
- (position EXP LIST [:test #'TESTFUNC]))
- (assoc OBJ LIST)
- (subst NEWITEM OLDITEM LIST)
- (subseq SEQUENCE INDEX)
- (elt SEQUENCE INDEX)

#### Cons

- (cons CAR CDR)

#### Variables

- (defparameter NAME VALUE)
- (setq NAME VALUE)
- (setf EXP NEWVALUE)
- (replaca CONS-EXP CAR-NEWVALUE)
- (replacd CONS-EXP CDR-NEWVALUE)
- (defvar NAME [VALUE])
- (let (NAME1 NAME2..) STATEMENTS)
- (let ((NAME1 VALUE1) (NAME2 VALUE2)...) STATEMENTS)
- (let\* (NAME1 NAME2..) STATEMENTS)
- (let\* ((NAME1 VALUE1) (NAME2 VALUE2)...) STATEMENTS)
- (defdyncamic NAME VALUE)
- (dynamic NAME)
- (dynamic-let (NAME1 NAME2..) STATEMENTS)
- (dynamic-let ((NAME1 VALUE1) (NAME2 VALUE2)...) STATEMENTS)

#### Operator

- (= EXP1 EXP2...)
- (/= EXP1 EXP2...)
- (\< EXP1 EXP2...)
- (\<= EXP1 EXP2...)
- (\> EXP1 EXP2...)
- (\>= EXP1 EXP2...)
- (eq EXP1 EXP2...)
- (eql EXP1 EXP2...)
- (equal EXP1 EXP2...)
- (equalp EXP1 EXP2...)
- (+ EXP1 EXP2...)
- (- EXP1 EXP2...)
- (\* EXP1 EXP2...)
- (/ EXP1 EXP2...)
- (mod EXP1 EXP2)
- (rem EXP1 EXP2)
- (1+ EXP)
- (1- EXP)
- (incf VAR)
- (incf VAR VALUE)
- (decf VAR)
- (decf VAR VALUE)
- (and EXP1 EXP2..)
- (or EXP1 EXP2..)
- (not EXP)

#### test

- (zerop EXP)
- (numberp EXP)
- (plusp EXP)
- (minusp EXP)
- (oddp EXP)
- (evenp EXP)
- (null EXP)
- (atom EXP)
- (integerp EXP)
- (floatp EXP)
- (symbolp EXP)
- (stringp EXP)
- (listp EXP)
- (consp EXP)

#### Coerce

- (coerce SEQUENCE 'list)
- (coerce SEQUENCE 'string)
- (coerce SEQUENCE 'utf8string)
- (coerce SEQUENCE 'utf32string)
- (to-utf8 "UTF32STRING")
- (to-utf32 "UTF8STRING")

#### Branch

- (if COND THEN-EXP ELSE-EXP)
- (when COND THEN-EXPs...)
- (unless COND ELSE-EXPs...)
- (cond (COND1 EXP1) (COND2 EXP2) ...)
- (case KEYFORM (KEY1 FORM1) (KEY2 FORM2) .... )

#### Loop

- (dotimes (VAR NUM) EXP1...)
- (dolist (VAR LIST) EXP1...)
- (while COND EXP1...)
- (for ((VAR INIT STEP)...) (COND RESULT) EXP1 EXP2...) like ISLISP
- (progn EXP1 EXP2...)
- (foreach (ITEMS) FORMS...)

#### Functions

- (defun NAME (\[PARAMVAR1... \]\[/ LOCALVAR1...\]) EXP1 EXP2...) like autolisp
    - &amp;rest; 
- (lambda (\[PARAMVAR1...\]\[/ LOCALVAR1...\]) EXP1 EXP2...) like autolisp

#### Constant

- most-postive-fixnum
- most-negative-fixnum
- pi

#### Function Reference

- (function FUNCTION)
- #'FUNCTION

#### Macro

- (defmacro NAME (PARAMS...) EXP1...)
- (macroexpand ..)

Backquotations and &amp;body are not supported.

#### Mapping

- (mapcar #'FUNCTION LIST)
- (apply #'FUNCTION [PARAMS...] LIST)
- (funcall #'FUNCTION EXP1...)
- (map 'list #'FUNCTION SEQUENCE)
- (map 'string #'FUNCTION SEQUENCE)

#### File I/O

- (with-open-file (HANDLE FILENAME  
    \[:direction :output|:input\]  
    \[:if-does-not-exist\] like CommonLisp
- (open "filename" "mode") like autolisp
- (close HANDLE)
- (command NAME ARG1...)
- (create-string-input-stream STRING)
- (create-string-output-stream)
- (get-output-stream-string STRSTREAM)

#### Standard I/O Handle

- (standard-input)
- (standard-output)
- (error-output)

#### Input functions

- (read [STREAM [EOF-FLAG [EOF-VALUE]]])
- (read-line [STREAM [EOF-FLAG [EOF-VALUE]]])
- (read-from-string [STRING]) == autolisp's read

#### Exceptions

- (block NAME FORM...)
    - (return-from NAME VALUE)
    - (return VALUE)
- (with-handler HANDLER FORM...) like ISLisp
- (unwind-protect FORM CLEANUP-FORM...)  like ISLisp
- (catch TAG FORM)
    - (throw TAG RESULT) like ISLisp

#### Quit

- (exit)
- (quit)
