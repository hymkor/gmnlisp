[![GoDoc](https://godoc.org/github.com/hymkor/gmnlisp?status.svg)](https://godoc.org/github.com/hymkor/gmnlisp)

Gmnlisp
=======

The Gmnlisp is a small Lisp implementation in Go.
The functions are the subset of ISLisp's.
It is developed to embbed to the applications for customizing.

( Now under constructing. Experimental implementing )

![Example image](factorial.png)

Install
-------

Download the binary package from [Releases](https://github.com/hymkor/gmnlisp/releases) and extract the executable.

### for scoop-installer

```
scoop install https://raw.githubusercontent.com/hymkor/gmnlisp/master/gmnlisp.json
```

or

```
scoop bucket add hymkor https://github.com/hymkor/scoop-bucket
scoop install gmnlisp
```

Examples
--------

### 1. Execute Lisp code in string with parameters

```go
<%
(defun string-replace (source from to)
  (let ((index nil) (buffer (create-string-output-stream)))
    (while (setq index (string-index from source))
      (format-object buffer (subseq source 0 index) nil)
      (format-object buffer to nil)
      (setq source (subseq source (+ index (length from)) (length source))))
    (format-object buffer source nil)
    (get-output-stream-string buffer)))

(defun quote-source (filename)
  (let ((line nil) (count 0))
    (with-open-input-file
      (fd filename)
      (while (setq line (read-line fd nil))
        (when (>= count 3)
          (setq line (string-replace line (create-string 1 #\tab) "    "))
          (format (standard-output) "~a~%" line))
        (incf count)))))

(quote-source "examples/example1.go")
%>
```

```
$ go run examples/example1.go
<% (command "go" "run" "examples/example1.go") %>
```

- `gmnlisp.New` returns the new Lisp interpretor instance (`*gmnlisp.World`).
- `gmnlisp.NewSymbol` is the symbol constructor. `gmnlisp.NewSymbol("a")` always returns the same value no matter how many times you call it.
- `gmnlisp.Variables` is the symbol-map type. It is the alias of `map[gmnlisp.Symbol]gmnlisp.Node`. `Node` is the interface-type that all objects in the Lisp have to implement.
- `.Let` makes a new instance including the given namespace.

```
lisp.Let(gmnlisp.Variables{
        gmnlisp.NewSymbol("a"): gmnlisp.Integer(1),
        gmnlisp.NewSymbol("b"): gmnlisp.Integer(2),
    }).Interpret(context.Background(),"(c)")
```

is same as `(let ((a 1) (b 1)) (c))`

### 2. Execute Lisp-code and give call-back function written in Go

```go
<% (quote-source "examples/example2.go") %>
```

```
$ go run examples/example2.go
<% (command "go" "run" "examples/example2.go") %>
```

The user defined normal functions have to get the three parameters.

- The 1st: context.Context that is given to the method .Interpret()
- The 2nd: \*gmnlisp.World on which the instance runs.
- The 3rd: []gmnlisp.Node the parameters given by caller. They are already evaluated.

`gmnlisp.Function` wraps the normal function as Lisp object.

- `F`: the function itself
- `C`: when the number of the parameter is not same as this, the error will be raised.
    If it does not have to be checked, omit it.

To get unevaluted parameters, the function's definition should be as below.

```
func FUNCNAME(c context.Context,w *World,args Node)(Node,error){...}
```

- The parameters are given as a list not an array.
- Use `gmnlisp.SpecialF(FUNCNAME)` instead of `gmnlisp.Function{F:FUNCNAME}`

Support Types
-------------

| Type(Lisp)        | Type(Go)
|-------------------|------------------------
| t                 | gmnlisp.\_TrueType
| nil               | gmnlisp.\_NullType
| &lt;integer&gt;   | gmnlisp.Integer == int64
| &lt;float&gt;     | gmnlisp.Float == float64
| &lt;string&gt;    | gmnlisp.String == string
| &lt;symbol&gt;    | gmnlisp.Symbol == int
| &lt;cons&gt;      | \*gmnlisp.Cons == struct{ Car,Cdr: gmnlisp.Node }
| &lt;character&gt; | gmnlisp.Rune == rune
| (keyword)         | gmnlisp.Keyword
| (array)           | \*gmnlisp.Array
| (hashtable)       | gmnlisp.\_Hash == map[gmnlisp.Node]gmnlisp.Node

`gmnlisp.Node` is the root interface.
All objects used in Lisp code have to satisfy it.

`gmnlisp.Symbol` is the unique number associated to string.

- string to gmnlisp.Symbol
    - `sbl := gmnlisp.NewSymbol("foo")`
- Symbol to string
    - `sbl.String()`

Support functions
-----------------

Gmnlisp's functions are subset of ISLisp.

#### List and Sequence

- (cons OBJ1 OBJ2)
- (car CONS)
- (cdr CONS)
- (quote OBJ)
- 'OBJ
- (list OBJ...)
- (rest LIST)
- (length SEQUENCE)
- (last LIST)
- (reverse LIST)
- (nreverse LIST)
- (append LIST...)
- (assoc OBJ LIST)
- (subseq SEQUENCE Z1 Z2)
- (elt SEQUENCE INDEX)
- (member ATOM LIST)
- (create-list I INITIAL-ELEMENT)

#### array

- #(...) , #2a((...) (...)) , #3a(((.. ..))) ...
- (create-array '(DIM...) INITIAL-VALUE)
- (array-dimensions ARRAY)
- (aref ARRAY INDEX...)
- (setf (aref ARAY INDEX) NEWVALUE)
- (set-aref NEWVALUE ARRAY INDEX...)
- (arrayp OBJ)

#### Variables

- (defdyncamic NAME FORM)
- (defglobal NAME FORM)
- (dynamic VAR)
- (dynamic-let ((VAR FORM)...) BODY-FORM...)
- (let ((VAR FORM)... ) BODY-FORM...)
- (let\* ((VAR FORM)...) BODY-FORM...)
- (set-car NEW-CAR CONS)
- (set-cdr NEW-CDR CONS)
- (setf PLACE FORM)
- (setq VAR FORM)

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
- (incf VAR [VALUE]) [MACRO]
- (decf VAR [VALUE]) [MACRO]
- (and EXP1 EXP2..)
- (or EXP1 EXP2..)
- (not EXP)
- (string= STRING1 STRING2)
- (string/= STRING1 STRING2)
- (string&lt; STRING1 STRING2)
- (string&gt; STRING1 STRING2)
- (string&gt;= STRING1 STRING2)
- (string&lt;= STRING1 STRING2)
- (string-index SUBSTRING STRING [START])
- (string-append STRING...)
- (create-string I [INITIAL-CHARACTER])
- (char= CHAR1 CHAR2)
- (char/= CHAR1 CHAR2)
- (char&lt; CHAR1 CHAR2)
- (char&gt; CHAR1 CHAR2)
- (char&gt;= CHAR1 CHAR2)
- (char&lt;= CHAR1 CHAR2)
- (characterp CHARACTER)
- (char-index CHAR STRING [START-POSITION])

#### test

- (atom OBJ)
- (consp OBJ)
- (evenp OBJ)
- (floatp OBJ)
- (functionp OBJ)
- (integerp OBJ)
- (listp OBJ)
- (minusp OBJ)
- (null OBJ)
- (numberp OBJ)
- (oddp OBJ)
- (plusp OBJ)
- (stringp OBJ)
- (symbolp OBJ)
- (zerop OBJ)

#### Convert

- (convert OBJ &lt;float&gt;)
- (convert OBJ &lt;integer&gt;)
- (convert OBJ &lt;list&gt;)
- (convert OBJ &lt;string&gt;)
- (convert OBJ &lt;symbol&gt;)
- (parse-number STRING)
- (truncate X)
- (floor X)
- (ceiling X)
- (round X)

#### Branch and Loop

- (case KEYFORM ((KEY...) FORM...)... [(t FORM...)])
- (case-using PREDFORM KEYFORM ((KEY...) FORM...)... [(t FORM...)])
- (cond (TEST FORM...)...)
- (for ((VAR INIT [STEP])...) (END-TEST RESULT...) FORM... )
- (if TEST-FORM THEN-FORM ELSE-FORM)
- (progn FORM...)
- (prog1 FORM...)
- (prog2 FORM...)
- (while TEST-FORM BODY-FORM...)
- (tagbody {TAG|FORM}...)
    - (go TAG)
- (dolist (VAR '(VALUES..)) FORM...) [MACRO]
- (dotimes (VAR N) FORM...) [MACRO]
- (when TEST-FORM THEN-FORM...)
- (unless TEST-FORM ELSE-FORM...)

#### Functions

- (lambda (IDENTIFIER... [&amp;rest IDENTIFIER]) FORM...)
- (defun FUNCTION-NAME (IDENTIFIER... [&amp;rest IDENTIFIER]) FORM...)
- (labels ((FUNCTION-NAME LAMBDA-LIST FORM...)...) BODY-FORM...)
- (flet ((FUNCTION-NAME LAMBDA-LIST FORM...)...) BODY-FORM...)

#### Constant

- most-postive-fixnum
- most-negative-fixnum
- pi

#### Function Reference

- (function FUNCTION)
- #'FUNCTION

#### Macro

- (defmacro NAME (IDENTIFIER... [&amp;rest IDENTIFIER]) FORM...)

#### Mapping

- (mapcar #'FUNCTION LIST)
- (mapc #'FUNCTION LIST)
- (mapcan #'FUNCTION LIST)
- (maplist #'FUNCTION LIST)
- (mapl #'FUNCTION LIST)
- (mapcon #'FUNCTION LIST)
- (apply #'FUNCTION [PARAMS...] LIST)
- (funcall #'FUNCTION EXP1...)

#### I/O

- (close STREAM)
- (create-string-input-stream STRING)
- (create-string-output-stream)
    - (get-output-stream-string STRSTREAM)
- (error-output)
- (file-length FILENAME ELEMENT-CLASS)
- (format {OUTPUT-STREAM|t|nil} FORMAT-STRING OBJ..)
    - (format t "..") is same as (format (standard-output) "..")
    - (format nil "..") is same as (let ((B create-string-output-stream)) (format B "..") (get-output-stream-string B))
- (format-char {OUTPUT-STREAM|t|nil} CHAR)
- (format-float {OUTPUT-STREAM|t|nil} FLOAT)
- (format-integer {OUTPUT-STREAM|t|nil} INTEGER RADIX)
- (format-object {OUTPUT-STREAM|t|nil} OBJ ESCAPE-P)
- (open-input-file FILENAME)
- (open-output-file FILENAME)
- (probe-file FILENAME)
- (read [STREAM [EOF-FLAG [EOF-VALUE]]])
- (read-line [STREAM [EOF-FLAG [EOF-VALUE]]])
- (standard-input)
- (standard-output)
- (with-open-input-file (NAME FILENAME) FORM...)
- (with-open-output-file (NAME FILENAME) FORM...)

#### Exceptions

- (block {SYMBOL|nil} FORM...)
    - (return RESULT-FORM)
    - (return-from SYMBOL RESULT-FORM)
- (catch TAG-FORM FORM...)
    - (throw TAG-FORM RESULT-FORM)
- (unwind-protect FORM CLEANUP-FORM...)
- (with-handler HANDLER FORM...)

#### Regular Expression

```
import (
    _ "github.com/hymkor/gmnlisp/regexp"
)
```

is required.

- (=~ REGEXP STRING)

compatible with "regexp".Regexp.FindAllStringSubmatch

``` lisp
(let ((m (=~ "a(x*)b" "-axxb-ab-")))
  (format t "ALL=~s~%" m)
  (format t "0,0=~s~%" (elt m 0 0))
  (format t "0,1=~s~%" (elt m 0 1))
  (format t "1,0=~s~%" (elt m 1 0))
  (format t "1,1=~s~%" (elt m 1 1))
  )
```

``` lisp
<%
(let ((m (=~ "a(x*)b" "-axxb-ab-")))
  (format t "ALL=~s~%" m)
  (format t "0,0=~s~%" (elt m 0 0))
  (format t "0,1=~s~%" (elt m 0 1))
  (format t "1,0=~s~%" (elt m 1 0))
  (format t "1,1=~s~%" (elt m 1 1))
  )
%>
```

- (=~i REGEXP STRING)

compatible with "regexp".Regexp.FindAllStringSubmatchIndex

``` lisp
(let ((m (=~i "a(x*)b" "-axxb-ab-")))
  (format t "INDEXES=~s~%" m)
  )
```

``` lisp
<%
(let ((m (=~i "a(x*)b" "-axxb-ab-")))
  (format t "INDEXES=~s~%" m)
  )
%>
```

#### Hash-table

```
(let ((h1 (make-hash-table)))
  (setf (gethash 'width h1) 600)
  (gethash 'width h1)
  (hash-table-count h1)
  (remhash 'width h1)
  (clrhash h1)
  )
```

#### Quit

- (exit)
- (quit)
- (abort)
