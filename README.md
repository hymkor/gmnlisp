[![GoDoc](https://pkg.go.dev/badge/github.com/hymkor/gmnlisp)][pkgGoDev]

Gmnlisp
=======

Gmnlisp is the interpreter of ISLisp written in Go.
It is developed to embbed to the applications for customizing.

[pkgGoDev]: https://pkg.go.dev/github.com/hymkor/gmnlisp

![Example image](factorial.png)

Examples
--------

### 1. Execute Lisp code in string with parameters

```examples/example1.go
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
    fmt.Println(value.String())
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

+ `a,err := gmnlisp.ExpectClass[gmnlisp.Integer](ctx,w,x)`  
  is similar as  
  `a,ok := x.(gmnlisp.Integer)`  
  but, ExpectClass can call error-handler defined by user when x is not Integer

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

Supported Types
---------------

Lisp values correspond to the following Go types or constructors when embedding gmnlisp in Go applications:

| Lisp         | Go                                      |
---------------|-----------------------------------------|
| `t`          | `gmnlisp.True`                          |
| `nil`        | `gmnlisp.Null`                          |
| `1`          | `gmnlisp.Integer(1)`                    |
| `2.3`        | `gmnlisp.Float(2.3)`                    |
| `"string"`   | `gmnlisp.String("string")`              |
| `Symbol`     | `gmnlisp.NewSymbol("Symbol")`           |
| `(cons 1 2)` | `&gmnlisp.Cons{ Car:gmnlisp.Integer(1), Cdr:gmnlisp.Integer(2) }` |
| `#\A`        | `gmnlisp.Rune('A')`                     |

Unlike other types shown above, `gmnlisp.NewSymbol(...)` is a function call, not a type conversion.
It returns a value of type `Symbol` (defined as `type Symbol int`), which is distinct from `int`.
The function guarantees that the same string always maps to the same symbol value.

`gmnlisp.Node` is the root interface.
All values that appear in Lisp code must implement this interface.

```
type Node interface {
    Equals(Node, EqlMode) bool
    String() string
    ClassOf() Class
}

type Class interface {
    Node
    Name() Symbol
    InstanceP(Node) bool
    Create() Node
    InheritP(Class) bool
}

type EqlMode int

const (
    STRICT EqlMode = iota // corresponds to (eql a b)
    EQUAL                 // corresponds to (equal a b)
    EQUALP                // corresponds to (equalp a b) 
)
```

ISLisp Compatibility
--------------------

Gmnlisp implements a subset of functions defined in the [ISLisp] standard.

The full compatibility checklist has been moved to a separate file due to its length:  
👉 [ISLisp Compatibility Checklist](./is_lisp_compat.md)

(Items without checkboxes are not standard functions.)

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

Author
------

[hymkor (HAYAMA Kaoru)](https://github.com/hymkor)

License
-------

[MIT Licence](./LICENSE)
