package gmnlisp

import (
	"context"
	"fmt"
	"math/big"
)

type Integer int64

var integerClass = registerClass(&BuiltInClass{
	name: NewSymbol("<integer>"),
	instanceP: func(n Node) bool {
		if _, ok := n.(Integer); ok {
			return true
		}
		_, ok := n.(BigInt)
		return ok
	},
	create: func() Node { return Integer(0) },
	super:  []Class{ObjectClass, numberClass},
})

func (i Integer) ClassOf() Class {
	return integerClass
}

func (i Integer) String() string {
	return fmt.Sprintf("%d", int64(i))
}

func (i Integer) Equals(n Node, m EqlMode) bool {
	if m == EQUALP {
		if _n, ok := n.(Integer); ok && i == _n {
			return true
		}
		_n, ok := n.(Float)
		return ok && Float(i) == _n
	} else {
		_n, ok := n.(Integer)
		return ok && i == _n
	}
}

func (i Integer) Add(ctx context.Context, w *World, n Node) (Node, error) {
	if _n, ok := n.(Float); ok {
		return Float(i) + _n, nil
	}
	_n, err := ExpectClass[Integer](ctx, w, n)
	if err == nil {
		return i + _n, nil
	}
	return nil, err
}

func (i Integer) Sub(ctx context.Context, w *World, n Node) (Node, error) {
	return promoteOperands(ctx, w, i, n,
		func(a, b Integer) (Node, error) {
			return a - b, nil
		},
		func(a, b BigInt) (Node, error) {
			return BigInt{Int: new(big.Int).Sub(a.Int, b.Int)}, nil
		},
		func(a, b Float) (Node, error) {
			return a - b, nil
		},
		func(a, b *big.Float) (Node, error) {
			v, _ := new(big.Float).Sub(a, b).Float64()
			return Float(v), nil
		})
}

func (i Integer) Multi(ctx context.Context, w *World, n Node) (Node, error) {
	if _n, ok := n.(BigInt); ok {
		return _n.Multi(ctx, w, i)
	} else if _n, ok := n.(Float); ok {
		return Float(i) * _n, nil
	}
	_n, err := ExpectClass[Integer](ctx, w, n)
	if err != nil {
		return nil, err
	}
	r := i * _n
	if _n != 0 && r/_n != i { // overflow
		return BigInt{Int: big.NewInt(int64(i))}.Multi(ctx, w, _n)
	}
	return r, nil
}

func (i Integer) LessThan(ctx context.Context, w *World, n Node) (bool, error) {
	if v, ok := n.(BigInt); ok {
		return big.NewInt(int64(i)).Cmp(v.Int) < 0, nil
	}
	if _n, ok := n.(Float); ok {
		return Float(i) < _n, nil
	}
	_n, err := ExpectClass[Integer](ctx, w, n)
	if err == nil {
		return i < _n, nil
	}
	return false, err
}
