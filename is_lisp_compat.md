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
- [x] with-error-output
- [x] with-open-input-file
- [x] with-open-io-file
- [x] with-open-output-file
- [x] with-standard-input
- [x] with-standard-output

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
- [x] with-error-output
- [x] with-handler
- [x] with-open-input-file
- [x] with-open-io-file
- [x] with-open-output-file
- [x] with-standard-input
- [x] with-standard-output

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
- [x] basic-vector-p
- [x] characterp
- [x] consp
- [x] floatp
- [x] functionp
- [x] general-array\*-p
- [x] general-vector-p
- [x] generic-function-p
- [x] integerp
- [x] listp
- [x] null
- [x] numberp
- [x] streamp
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
- [x] reciprocal
- [x] quotient
- [x] max
- [x] min
- [x] abs
- [x] exp
- [x] log
- [x] expt
- [x] sqrt
- [x] sin
- [x] cos
- [x] tan
- [x] atan
- [ ] atan2
- [x] sinh
- [x] cosh
- [x] tanh
- [x] atanh
- 1+
- 1-
- incf
- decf

#### 11.2 Float class

- [x] \*pi\*
- [x] \*most-positive-float\*
- [x] \*most-negative-float\*
- [x] floatp
- [x] float
- [x] floor
- [x] ceiling
- [x] truncate
- [x] round

#### 11.3 Integer class

- [x] integerp
- [x] div
- [x] mod
- [x] gcd
- [x] lcm
- [x] isqrt
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
- [x] garef
- [x] set-aref , (setf (aref BASIC-ARRAY Z\*) OBJ)
- [x] set-garef , (setf (garef BASIC-ARRAY Z\*) OBJ)
- [x] array-dimensions
- #(...) , #2a((...) (...)) , #3a(((.. ..))) ...

### 15 Vector

- [x] basic-vector-p
- [x] general-vector-p
- [x] create-vector
- [x] vector

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
- [x] map-into

### 18 Stream class

- [x] streamp
- [x] open-stream-p
- [x] input-stream-p
- [x] output-stream-p
- [x] standard-input
- [x] standard-output
- [x] error-output
- [x] with-standard-input
- [x] with-standard-output
- [x] with-error-output

#### 18.1 Streams to files

- [x] open-input-file
- [x] open-output-file
- [x] open-io-file
- [x] with-open-input-file
- [x] with-open-output-file
- [x] with-open-io-file
- [x] close
- [x] finish-output

#### 18.2 Other streams

- [x] create-string-input-stream
- [x] create-string-output-stream
- [x] get-output-stream-string

### 19 Input and Output

#### 19.1 Argument conventions for input functions

- [x] read
- [x] read-char
- [x] preview-char
- [x] read-line
- [x] stream-ready-p
- [x] format
- [x] format-char
- [x] format-float
- [x] format-fresh-line
- [x] format-integer
- [x] format-object
- [x] format-tab

#### 19.2 Charactoer I/O

#### 19.3 Binary I/O

- [x] read-byte
- [x] write-byte

### 20 Files

- [x] probe-file
- [x] file-position
- [x] set-file-position
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
- [x] condition-continuable
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

- [x] parse-error-string
- [x] parse-error-expected-class

##### 21.3.4 Simple errors

- [x] simple-error-format-string
- [x] simple-error-format-arguments

##### 21.3.5 Stream errors

- [x] stream-error-stream
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
- [x] \<parse-error\>, cannot-parse-number
- [x] \<control-error\>, control-error
- [x] \<devision-by-zero\>, division-by-zero
- [x] \<domain-error\>
    - not-an-input-stream
    - not-an-output-stream
- [x] \<end-of-stream\>, end-of-stream
- [x] \<undefined-entity\> , undefined-entity
    - [x] \<unbound-variable\> , unbound-variable
    - [x] \<undefined-function\>, undefined-function

### 22 Miscellaneous

- [x] identify
- [x] get-universal-time
- [x] get-internal-real-time
- [x] get-internal-run-time
- [x] internal-time-units-per-second

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

