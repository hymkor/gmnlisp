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

integer , float , string , symbol , cons , list , character , t/nil

Support functions
-----------------

`*`,
`*err-exist*`,
`*err-not-exist*`,
`*err-quit*`,
`*err-too-few-arguments*`,
`*err-too-many-arguments*`,
`*err-too-short-tokens*`,
`*err-variable-unbound*`,
`+`,
`-`,
`--get-all-symbols--`,
`/`,
`/=`,
`1+`,
`1-`,
`<`,
`<=`,
`=`,
`>`,
`>=`,
`and`,
`append`,
`apply`,
`aref`,
`assoc`,
`atom`,
`block`,
`cadddr`,
`caddr`,
`cadr`,
`car`,
`case`,
`catch`,
`cdddr`,
`cddr`,
`cdr`,
`close`,
`coerce`,
`command`,
`concatenate`,
`cond`,
`cons`,
`consp`,
`decf`,
`defdynamic`,
`defmacro`,
`defparameter`,
`defun`,
`defvar`,
`detab`,
`dolist`,
`dotimes`,
`dynamic`,
`dynamic-let`,
`elt`,
`eq`,
`eql`,
`equal`,
`equalp`,
`error-output`,
`evenp`,
`exit`,
`find`,
`first`,
`floatp`,
`for`,
`foreach`,
`format`,
`funcall`,
`function`,
`if`,
`incf`,
`integerp`,
`lambda`,
`last`,
`length`,
`let`,
`let*`,
`list`,
`listp`,
`load`,
`macroexpand`,
`map`,
`mapcar`,
`member`,
`minusp`,
`mod`,
`most-negative-fixnum`,
`most-positive-fixnum`,
`nil`,
`not`,
`nth`,
`nthcdr`,
`null`,
`numberp`,
`oddp`,
`open`,
`or`,
`parse-integer`,
`pi`,
`plusp`,
`position`,
`prin1`,
`princ`,
`print`,
`progn`,
`quit`,
`quote`,
`read`,
`read-line`,
`rem`,
`replaca`,
`replacd`,
`rest`,
`return`,
`return-from`,
`reverse`,
`second`,
`setf`,
`setq`,
`split-string`,
`standard-input`,
`standard-output`,
`strcase`,
`strcat`,
`stringp`,
`strlen`,
`subseq`,
`subst`,
`substr`,
`symbolp`,
`t`,
`terpri`,
`third`,
`throw`,
`trace`,
`truncate`,
`typep`,
`unless`,
`unwind-protect`,
`when`,
`while`,
`with-handler`,
`with-open-file`,
`write`,
`write-line`,
`zerop`
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

#### string

- (concatenate 'string {STRING1...})
- (aref STRING INDEX)

#### List

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
- (append LIST...)
- (find ATOM LIST [:test #'TESTFUNC])
- (member ATOM LIST [:test #'TESTFUNC])

#### Cons

- (cons CAR CDR)

#### Variables

- (defparameter NAME VALUE)
- (setq NAME VALUE)
- (defvar NAME [VALUE])
- (let (NAME1 NAME2..) STATEMENTS)
- (let ((NAME1 VALUE1) (NAME2 VALUE2)...) STATEMENTS)

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
- (1+ EXP)
- (1- EXP)
- (incf VAR)
- (incf VAR VALUE)
- (decf VAR)
- (decf VAR VALUE)

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

#### Functions

- (defun NAME (\[PARAMVAR1... \]\[/ LOCALVAR1...\]) EXP1 EXP2...) like autolisp
    - &amp;rest; 
- (lambda (\[PARAMVAR1...\]\[/ LOCALVAR1...\]) EXP1 EXP2...) like autolisp

#### Constant

- most-postive-fixnum like CommonLisp
- most-negative-fixnum like CommonLisp
- pi like CommonLisp

#### Function Reference

- (function FUNCTION)
- #'FUNCTION

#### Macro

- (defmacro NAME (PARAMS...) EXP1...)
- (macroexpand ..)
- not support backquotation and &amp;body

#### Mapping

- (mapcar #'FUNCTION LIST)
- (apply #'FUNCTION [PARAMS...] LIST)
- (funcall #'FUNCTION EXP1...)
- (map 'list #'FUNCTION SEQUENCE)
- (map 'string #'FUNCTION SEQUENCE)

#### File I/O

- (with-open-file (HANDLER FILENAME  
    \[:direction :output|:input\]  
    \[:if-does-not-exist\] like CommonLisp
- (open "filename" "mode") like autolisp

#### Standard I/O

- (standard-input)
- (standard-output)
- (error-output)

#### Input functions

- (read [STRING]) like autolisp
- (read-line [STREAM [EOF-FLAG [EOF-VALUE]]]) like CommonLisp

#### Exceptions

- (block NAME FORM...)
    - (return-from NAME VALUE)
    - (return VALUE)
- (with-handler HANDLER FORM...) like ISLisp
- (unwind-protect FORM CLEANUP-FORM...)  like ISLisp
- (catch TAG FORM)
    - (throw TAG RESULT) like ISLisp

- (quit)
