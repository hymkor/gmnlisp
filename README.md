Gmnlisp
=======

[![Go Reference](https://pkg.go.dev/badge/github.com/hymkor/gmnlisp.svg)](https://pkg.go.dev/github.com/hymkor/minipage)
[![Go Test](https://github.com/hymkor/gmnlisp/actions/workflows/go.yml/badge.svg)](https://github.com/hymkor/minipage/actions/workflows/go.yml)

Gmnlisp is the interpreter of ISLisp written in Go.
It is developed to embbed to the applications for customizing.

[pkgGoDev]: https://pkg.go.dev/github.com/hymkor/gmnlisp

![Example image](factorial.png)

Examples
--------

```examples/example.go
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

    lisp = lisp.Let(gmnlisp.Variables{
        gmnlisp.NewSymbol("a"): gmnlisp.Integer(1),
        gmnlisp.NewSymbol("b"): gmnlisp.Integer(2),
    })

    lisp = lisp.Flet(
        gmnlisp.Functions{
            gmnlisp.NewSymbol("sum"): &gmnlisp.Function{C: 2, F: sum},
        })

    value, err := lisp.Interpret(context.TODO(), "(sum a b)")
    if err != nil {
        fmt.Fprintln(os.Stderr, err.Error())
        return
    }
    fmt.Println(value.String())
}
```

```
$ go run examples/example.go
3
```

- `gmnlisp.New` returns a new Lisp interpreter instance (`*gmnlisp.World`).
- `gmnlisp.NewSymbol` constructs a symbol. Calling `gmnlisp.NewSymbol("a")` always returns the same value, no matter how many times it's called.
- `gmnlisp.Variables` is a symbol map type. It is an alias for `map[gmnlisp.Symbol]gmnlisp.Node`.  
  `Node` is the interface that all Lisp objects must implement.
- `.Let` creates a new world instance with the given variable bindings (namespace).

```lisp
lisp.Let(gmnlisp.Variables{
    gmnlisp.NewSymbol("a"): gmnlisp.Integer(1),
    gmnlisp.NewSymbol("b"): gmnlisp.Integer(2),
}).Interpret(context.Background(), "(c)")

```

is equivalent to the Lisp code: `(let ((a 1) (b 2)) (c))`

### Type assertions:

`a, err := gmnlisp.ExpectClass[gmnlisp.Integer](ctx, w, x)`  
is similar to:  
`a, ok := x.(gmnlisp.Integer)`  

However, `ExpectClass` invokes the user-defined error handler if `x` is not of type `Integer`.

### User-defined functions:

User-defined functions must accept three parameters:

1. `context.Context`: the context passed to `.Interpret()`
2. `*gmnlisp.World`: the interpreter instance
3. `[]gmnlisp.Node`: the arguments passed by the caller (already evaluated)

Such a function can be wrapped as a Lisp value like this:  
`&gmnlisp.Function{C: argc, F: function}`

- Field `F`: the function itself
- Field `C`: the required number of arguments. An error is raised if the number of arguments doesn't match.

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

### Gmnlisp and other implementations of [ISLisp]

| Implementation      | Language      |  Windows  | Linux     | Execution Model      |
|---------------------|---------------|-----------|-----------|----------------------|
| [OK!ISLisp][oki]    | C             | Supported | Supported | Interpreter/Bytecode compiler |
| [iris]              | Go/JavaScript | Supported | Supported | Interpreter          |
| [Easy-ISLisp][eisl] | C             |           | Supported | Interpreter/Native Compiler |
| **gmnlisp**         | Go            | Supported | Supported | Interpreter          |

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
