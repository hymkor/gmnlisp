package gmnlisp

import (
	"context"
	"fmt"
	"io"
	"strings"

	. "github.com/hymkor/gmnlisp"
)

func init() {
	Export(NewSymbol("clrhash"), Function1(funClearHash))
	Export(NewSymbol("gethash"), Function2(funGetHash))
	Export(NewSymbol("hash-table-count"), Function1(funHashTableCount))
	Export(NewSymbol("make-hash-table"), Function0(funMakeHashTable))
	Export(NewSymbol("remhash"), Function2(funRemoveHash))
	Export(NewSymbol("set-gethash"), &Function{C: 3, F: funSetHash})
}

type _Hash map[Node]Node

func (h _Hash) Equals(other Node, mode EqlMode) bool {
	return false
}

func (h _Hash) PrintTo(w io.Writer, mode PrintMode) (int, error) {
	n := 0
	dem := '{'
	for key, val := range h {
		_n, err := fmt.Fprintf(w, "%c%#v:%#v", dem, key, val)
		n += _n
		if err != nil {
			return n, err
		}
		dem = ','
	}
	_n, err := w.Write([]byte{'}'})
	n += _n
	return n, err
}

var hashClass = NewBuiltInClass[_Hash]("<hashtable>")

func (_Hash) ClassOf() Class {
	return hashClass
}

func (h _Hash) Eval(ctx context.Context, w *World) (Node, error) {
	return h, nil
}

func funMakeHashTable(ctx context.Context, w *World) (Node, error) {
	return make(_Hash), nil
}

func canUseHashKey(value Node) bool {
	switch value.(type) {
	case String:
		return true
	case Symbol:
		return true
	case Integer:
		return true
	case Float:
		return true
	case Rune:
		return true
	case Keyword:
		return true
	}
	return false
}

func funGetHash(ctx context.Context, w *World, first, second Node) (Node, error) {
	hash, err := ExpectClass[_Hash](ctx, w, second)
	if err != nil {
		return nil, err
	}
	if !canUseHashKey(first) {
		return nil, ErrNotSupportType
	}
	value, ok := hash[first]
	if !ok {
		return Null, nil
	}
	return value, nil
}

func funSetHash(ctx context.Context, w *World, args []Node) (Node, error) {
	hash, err := ExpectClass[_Hash](ctx, w, args[2])
	if err != nil {
		return nil, err
	}
	if !canUseHashKey(args[1]) {
		return nil, ErrNotSupportType
	}
	hash[args[1]] = args[0]
	return args[0], nil
}

func funHashTableCount(ctx context.Context, w *World, arg Node) (Node, error) {
	hash, err := ExpectClass[_Hash](ctx, w, arg)
	if err != nil {
		return nil, err
	}
	return Integer(len(hash)), nil
}

func funRemoveHash(ctx context.Context, w *World, first, second Node) (Node, error) {
	hash, err := ExpectClass[_Hash](ctx, w, second)
	if err != nil {
		return nil, err
	}
	if !canUseHashKey(first) {
		return nil, ErrNotSupportType
	}
	delete(hash, first)
	return Null, nil
}

func funClearHash(ctx context.Context, w *World, arg Node) (Node, error) {
	hash, err := ExpectClass[_Hash](ctx, w, arg)
	if err != nil {
		return nil, err
	}
	for key := range hash {
		delete(hash, key)
	}
	return Null, nil
}

func (t _Hash) String() string {
	var buffer strings.Builder
	t.PrintTo(&buffer, PRINC)
	return buffer.String()
}

func (t _Hash) GoString() string {
	var buffer strings.Builder
	t.PrintTo(&buffer, PRINT)
	return buffer.String()
}
