package gmnlisp

import (
	"context"
	"math/big"
)

type BigInt struct {
	*big.Int
}

func (b BigInt) Equals(n Node, m EqlMode) bool {
	if bi, ok := n.(BigInt); ok {
		return b.Int.Cmp(bi.Int) == 0
	}
	if _n, ok := n.(Integer); ok {
		bi := big.NewInt(int64(_n))
		return b.Int.Cmp(bi) == 0
	}
	return false
}

func (b BigInt) ClassOf() Class {
	return integerClass
}

func (b BigInt) Multi(ctx context.Context, w *World, other Node) (Node, error) {
	return promoteOperands(ctx, w, b, other,
		func(a, b Integer) (Node, error) {
			return a * b, nil
		},
		func(a, b BigInt) (Node, error) {
			return BigInt{Int: new(big.Int).Mul(a.Int, b.Int)}, nil
		},
		func(a, b Float) (Node, error) {
			return Float(a * b), nil
		},
		func(a, b *big.Float) (Node, error) {
			v, _ := new(big.Float).Mul(a, b).Float64()
			return Float(v), nil
		})
}

func (b BigInt) Sub(ctx context.Context, w *World, other Node) (Node, error) {
	var o *big.Int

	if i, ok := other.(Integer); ok {
		o = big.NewInt(int64(i))
	} else if b2, ok := other.(BigInt); ok {
		o = b2.Int
	} else {
		return nil, &DomainError{
			Reason: "not a integer or bigint",
			Object: other,
		}
	}
	return BigInt{Int: new(big.Int).Sub(b.Int, o)}, nil
}

func (b BigInt) LessThan(ctx context.Context, w *World, other Node) (bool, error) {
	var o BigInt
	var err error

	if v, ok := other.(Integer); ok {
		o = BigInt{Int: big.NewInt(int64(v))}
	} else if o, err = ExpectClass[BigInt](ctx, w, other); err != nil {
		return false, err
	}
	return b.Int.Cmp(o.Int) < 0, nil
}
