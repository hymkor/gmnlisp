[![GoDoc](https://pkg.go.dev/badge/github.com/hymkor/gmnlisp)][pkgGoDev]

Gmnlisp
=======

The Gmnlisp is a small Lisp implementation in Go.
The functions are the subset of [ISLisp]'s.
It is developed to embbed to the applications for customizing.

[pkgGoDev]: https://pkg.go.dev/github.com/hymkor/gmnlisp

![Example image](factorial.png)

Install
-------

Download the binary package from [Releases](https://github.com/hymkor/gmnlisp/releases) and extract the executable.

### via golang-installer

```
go install github.com/hymkor/gmnlisp/cmd/gmnlisp@latest
```

### via scoop-installer

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
package main

import (
    "context"
    "fmt"
    "os"

    "github.com/hymkor/gmnlisp"
)

func main() {
    lisp := gmnlisp.New()
    lisp = lisp.Let(gmnlisp.Variables{
        gmnlisp.NewSymbol("a"): gmnlisp.Integer(1),
        gmnlisp.NewSymbol("b"): gmnlisp.Integer(2),
    })
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

```examples/example2.go
package main

import (
    "context"
    "fmt"
    "os"

    "github.com/hymkor/gmnlisp"
)

//    `a,err := gmnlisp.ExpectClass[gmnlisp.Integer](ctx,w,x)`
// is similar as
//    `a,ok := x.(gmnlisp.Integer)`
// but, ExpectClass can call error-handler defined by user when x is not Integer

func sum(ctx context.Context, w *gmnlisp.World, args []gmnlisp.Node) (gmnlisp.Node, error) {
    a, err := gmnlisp.ExpectClass[gmnlisp.Integer](ctx, w, args[0])
    if err != nil {
        return nil, err
    }
    b, err := gmnlisp.ExpectClass[gmnlisp.Integer](ctx, w, args[1])
    if err != nil {
        return nil, err
    }
    return a + b, nil
}

func main() {
    lisp := gmnlisp.New()
    lisp = lisp.Flet(
        gmnlisp.Functions{
            gmnlisp.NewSymbol("sum"): &gmnlisp.Function{C: 2, F: sum},
        })

    result, err := lisp.Interpret(context.Background(), `(sum 1 2)`)
    if err != nil {
        fmt.Fprintln(os.Stderr, err.Error())
        return
    }
    fmt.Printf("(sum 1 2)=%v\n", result)
}
```

```
$ go run examples/example2.go
(sum 1 2)=3
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

Gmnlisp's functions are subset of [ISLisp].

Items without checkboxes are not standard functions

### 1 Scope, Conventions and Compliance

### 2 Classes

### 3 Scope and Extent

#### 3.1 The lexical Principle
#### 3.2 Scope of Identifiers
#### 3.3 Some Specific Scope Rules
#### 3.4 Extent

- [x] block
- [x] dynamic-let
- [x] flet
- [x] for
- [x] labels
- [x] let
- [x] let\*
- [x] tagbody
- [ ] with-error-output
- [ ] with-open-input-file
- [ ] with-open-io-file
- [x] with-open-output-file
- [x] with-standard-input
- [ ] with-standard-output

### 4 Forms and Evaluation

#### 4.1 Forms
#### 4.2 Function Application Forms
#### 4.3 Special Forms

- [x] and
- [x] assure
- [x] block
- [x] case
- [x] case-using
- [x] catch
- [x] class
- [x] cond
- [x] convert
- [x] dynamic
- [x] dynamic-let
- [x] flet
- [x] for
- [x] function
    - [x] &amp;rest
    - #'FUNCTION
- [x] go
- [x] if
- [x] ignore-errors
- [x] labels
- [x] lambda
- [x] let
- [x] let\*
- [x] or
- [x] progn
- [x] quote
- [x] return-from
- [x] setf
- [x] setq
- [x] tagbody
- [x] the
- [x] throw
- [x] unwind-protect
- [x] while
- [ ] with-error-output
- [x] with-handler
- [ ] with-open-input-file
- [ ] with-open-io-file
- [x] with-open-output-file
- [x] with-standard-input
- [ ] with-standard-output

#### 4.4 Defining Forms

- [x] defclass
    - [x] :initarg
    - [x] :initform
    - [x] :accessor
    - [x] :reader
    - [x] :writer
    - [x] :boundp
- [x] defconstant (defined same as defglobal for dummy)
- [x] defdynamic
- [x] defgeneric
- [x] defglobal
- [x] defmacro
- [x] defmethod
- [x] defun

#### 4.5 Macro Forms
#### 4.6 The Evaluation Model
#### 4.7 Functions

- [x] functionp
- [x] function
- [x] lambda
- [x] labels
- [x] flet
- [x] apply
- [x] funcall

#### 4.8 Defining Operators

- [x] defconstant
- [x] defglobal
- [x] defdynamic
- [x] defun

### 5 Predicates

#### 5.1 Boolean Values

- [x] t
- [x] nil

#### 5.2 Class Predicates

- [x] basic-array-p
- [x] basic-array\*-p
- [ ] basic-vector-p
- [x] characterp
- [x] consp
- [x] floatp
- [x] functionp
- [x] general-array\*-p
- [ ] general-vector-p
- [x] generic-function-p
- [x] integerp
- [x] listp
- [x] null
- [x] numberp
- [ ] streamp
- [x] stringp
- [x] symbolp
- atom OBJ
- evenp OBJ
- minusp OBJ
- oddp OBJ
- plusp OBJ
- zerop OBJ

#### 5.3 Equality

- [x] eq
- [x] eql
- [x] equal
- equalp

#### 5.4 Logical Connectives

- [x] not
- [x] and
- [x] or

### 6 Control Structure
#### 6.1 Constants

- [x] \(quote\)
- [x] 'OBJ

#### 6.2 Variables

- [x] setq
- [x] setf
- [x] let
- [x] let\*

#### 6.3 Dynamic Variables

- [x] dynamic
- [x] dynamic-let

#### 6.4 Conditional Expressions

- [x] if
- [x] cond
- [x] case
- [x] case-using
- when
- unless

#### 6.5 Sequencing Forms

- [x] progn
- prog1
- prog2

#### 6.6 Iteration

- [x] while
- [x] for
- dolist
- dotimes

#### 6.7 Non-Local Exits

##### 6.7.1 Establishing and Invoking Non-Local Exits

- [x] block
- [x] return-from
- [x] catch
- [x] throw
- [x] tagbody
- [x] go
- return

##### 6.7.2 Assuring Data Consistency during Non-Local Exists

- [x] unwind-protect

### 7 Objects

#### 7.1 Defining Classes

- [x] defclass
- [x] generic-function-p

#### 7.2 Generic Functions

##### 7.2.1 Defining Generic Functions

- [x] defgeneric
- [x] :rest, &amp;rest

##### 7.2.2 Defining Methods for Generic Functions

- [x] defmethod
    - [x] :rest, &amp;rest

#### 7.3 Calling Generic Functions

##### 7.3.4 Calling More General Methods

- [ ] call-next-method
- [ ] next-method-p

#### 7.4 Object Creation and Initialization

- [x] create
    - [x] :initarg
    - [x] :initform
    - [x] :accessor
    - [x] :reader
    - [x] :writer
    - [x] :boundp
- [x] initialize-object

#### 7.5 Class Enquiry

- [x] class-of
- [x] instancep
- [x] subclassp
- [x] class

### 8 Macros

- [x] defmacro
- [x] 'form
- [x] \`form
- [x] ,@form

### 9 Declarations and Coercions

- [x] the
- [x] assure
- [x] convert
    - [x] \(convert OBJ &lt;float&gt;)
    - [x] \(convert OBJ &lt;integer&gt;)
    - [x] \(convert OBJ &lt;list&gt;)
    - [x] \(convert OBJ &lt;string&gt;)
    - [x] \(convert OBJ &lt;symbol&gt;)

### 10 Symbol classes

- [x] symbolp

#### 10.2 Symbol Properties

- [x] property
- [x] set-property , setf
- [x] remove-property

#### 10.3 Unnamed Symbols

- [x] gensym

### 11 Number class

#### 11.1 Number class

- [x] numberp
- [x] parse-number
- [x] =
- [x] /=
- [x] \>=
- [x] \<=
- [x] \>
- [x] \<
- [x] +
- [x] \*
- [x] -
- [ ] reciproca1
- [ ] quotient
- [x] max
- [x] min
- [ ] abs
- [ ] exp
- [ ] log
- [ ] expt
- [x] sqrt
- [ ] sin
- [ ] cos
- [ ] tan
- [ ] atan
- [ ] atan2
- [ ] sinh
- [ ] cosh
- [ ] tanh
- [ ] atanh
- 1+
- 1-
- incf
- decf

#### 11.2 Float class

- [ ] \*pi\*
- [ ] \*most-positive-float\*
- [ ] \*most-negative-float\*
- [x] floatp
- [ ] float
- [x] floor
- [x] ceiling
- [x] truncate
- [x] round

#### 11.3 Integer class

- [x] integerp
- [ ] div
- [x] mod
- [ ] gcd
- [ ] lcm
- [ ] isqrt
- rem
- most-postive-fixnum
- most-negative-fixnum

### 12 Character class

- [x] characterp
- [x] char=
- [x] char/=
- [x] char\<
- [x] char\>
- [x] char\<=
- [x] char\>=

### 13 List class

#### 13.1 Cons

- [x] consp
- [x] cons OBJ
- [x] car
- [x] cdr
- [x] set-car, (setf (car CONS) OBJ)
- [x] set-cdr, (setf (cdr CONS) OBJ)

#### 13.2 Null class

- [x] null

#### 13.3 List operations

- [x] listp
- [x] create-list
- [x] list
- [x] reverse
- [x] nreverse
- [x] append
- [x] member
- [x] mapcar
- [x] mapc
- [x] maplist
- [x] mapl
- [x] mapcan
- [x] mapcon
- [x] assoc
- last

### 14 Arrays

#### 14.1 Array Classes

#### 14.2 General Arrays

#### 14.3 Array Operations

- [x] basic-array-p
- [x] basic-array\*-p
- [x] general-array\*-p
- [x] create-array
- [x] aref
- [ ] garef
- [x] set-aref , (setf (aref BASIC-ARRAY Z\*) OBJ)
- [ ] set-garef , (setf (garef BASIC-ARRAY Z\*) OBJ)
- [x] array-dimensions
- #(...) , #2a((...) (...)) , #3a(((.. ..))) ...

### 15 Vector

- [ ] basic-vector-p
- [ ] general-vector-p
- [ ] create-vector
- [ ] vector

### 16 String class

- [x] stringp
- [x] create-string
- [x] string=
- [x] string/=
- [x] string\<
- [x] string\>
- [x] string\>=
- [x] string\<=
- [x] char-index
- [x] string-index
- [x] string-append

### 17 Sequence Functions

- [x] length
- [x] elt
- [x] set-elt, (setf (elt SEQ Z) OBJ)
- [x] subseq
- [ ] map-into

### 18 Stream class

- [ ] streamp
- [ ] open-stream-p
- [ ] input-stream-p
- [ ] output-stream-p
- [x] standard-input
- [x] standard-output
- [x] error-output
- [ ] with-standard-input
- [ ] with-standard-output
- [ ] with-error-output

#### 18.1 Streams to files

- [x] open-input-file
- [x] open-output-file
- [ ] open-io-file
- [x] with-open-input-file
- [x] with-open-output-file
- [ ] with-open-io-file
- [x] close
- [ ] finish-output

#### 18.2 Other streams

- [x] create-string-input-stream
- [x] create-string-output-stream
- [x] get-output-stream-string

### 19 Input and Output

#### 19.1 Argument conventions for input functions

- [x] read
- [ ] read-char
- [ ] preview-char
- [x] read-line
- [ ] stream-ready-p
- [x] format
- [x] format-char
- [x] format-float
- [ ] format-fresh-line
- [x] format-integer
- [x] format-object
- [ ] format-tab

#### 19.2 Charactoer I/O

#### 19.3 Binary I/O

- [ ] read-byte
- [ ] write-byte

### 20 Files

- [x] probe-file
- [ ] file-position
- [ ] set-file-position
- [x] file-length

### 21 Condition System

#### 21.1 Condition
#### 21.2 Signaling and handling condtions
##### 21.2.1 Operations relating to condition signaling

- [x] error
- [x] cerror
- [x] signal-condition

##### 21.2.2 Operations relating to condition handling

- [x] ignore-error
- [x] report-condition
- [ ] condition-continuable
- [x] continue-condition
- [x] with-handler

#### 21.3 Data associated with condition classes
##### 21.3.1 Arithmetic errors

- [x] arithmetic-error-operation
- [x] arithmetic-error-operands

##### 21.3.2 Domain errors

- [x] domain-error-object
- [x] domain-error-expected-class

##### 21.3.3 Parse errors

- [ ] parse-error-string
- [ ] parse-error-expected-class

##### 21.3.4 Simple errors

- [x] simple-error-format-string
- [x] simple-error-format-arguments

##### 21.3.5 Stream errors

- [ ] stream-error-stream
- [x] undefined-entity-name
- [x] undefined-entity-namespace

#### 21.4 Error identification

- [x] \<program-error\>
    - [x] arity-error
    - [ ] immutable-binding
    - [ ] improper-argument-binding
    - [ ] index-out-of-range
- [ ] \<storage-exhausted\>
    - [ ] cannot-create-array
    - [ ] cannot-create-array
    - [ ] cannot-create-cons
    - [ ] cannot-create-list
    - [ ] cannot-create-sequence
    - [ ] cannot-create-string
    - [ ] cannot-create-vector
- [ ] \<parse-error\>, cannot-parse-number
- [ ] \<control-error\>, control-error
- [x] \<devision-by-zero\>, division-by-zero
- [x] \<domain-error\>
    - not-an-input-stream
    - not-an-output-stream
- [ ] \<end-of-stream\>, end-of-stream
- [x] \<undefined-entity\> , undefined-entity
    - [x] \<unbound-variable\> , unbound-variable
    - [x] \<undefined-function\>, undefined-function

### 22 Miscellaneous

- [ ] identify
- [ ] get-universal-time
- [ ] get-internal-real-time
- [ ] get-internal-run-time
- [ ] internal-time-units-per-second

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

References
----------

### Documents (English)

+ [ISLISP - Wikipedia](https://en.wikipedia.org/wiki/ISLISP)
+ [ISLisp Home Page][ISLisp]
+ [www.islisp.info: Home](http://www.islisp.info/)
+ [Programming Language ISLISP Working Draft 23.0](https://nenbutsu.github.io/ISLispHyperDraft/islisp-v23.html)

### Documents (Japanese)

+ [JISX3012:1998 プログラム言語ＩＳＬＩＳＰ](https://kikakurui.com/x3/X3012-1998-01.html)
+ [M.Hiroi's Home Page / お気楽 ISLisp プログラミング超入門](http://www.nct9.ne.jp/m_hiroi/clisp/islisp.html)

### Other implementations of [ISLisp]

|               | Language|  Windows  | Linux
|---------------------|---|-----------|----------
| [OK!ISLisp][oki]    | C | Supported | ?
| [iris]              | Go| Supported | Supported
| [Easy-ISLisp][eisl] | C |           | Supported

[ISLisp]: http://islisp.org/
[oki]: http://islisp.org/OKIISLisp.html
[iris]: https://github.com/islisp-dev/iris
[eisl]: https://github.com/sasagawa888/eisl
