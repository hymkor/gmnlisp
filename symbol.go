package gmnlisp

import (
	"context"
	"fmt"
)

type idMap[T ~int] struct {
	id2name []string
	name2id map[string]T
}

func (idm *idMap[T]) NameToId(name string) T {
	if idm.name2id == nil {
		idm.name2id = make(map[string]T)
	}
	if id, ok := idm.name2id[name]; ok {
		return id
	}
	id := T(len(idm.name2id))
	idm.name2id[name] = id
	idm.id2name = append(idm.id2name, name)
	return id
}

func (idm *idMap[T]) Count() int {
	return len(idm.name2id)
}

func (idm *idMap[T]) IdToName(id T) string {
	if id < 0 || int(id) >= len(idm.id2name) {
		return "(undefined)"
	}
	return idm.id2name[id]
}

type Symbol interface {
	Id() int
	Node
}

type _Symbol int

func (s _Symbol) Id() int {
	return int(s)
}

func (_ _Symbol) ClassOf() Class {
	return symbolClass
}

func (s _Symbol) Eval(_ context.Context, w *World) (Node, error) {
	return w.Get(s)
}

func (s _Symbol) Equals(n Node, m EqlMode) bool {
	ns, ok := n.(_Symbol)
	return ok && s == ns
}

func (s _Symbol) String() string {
	return symbolManager.IdToName(s)
}

var symbolManager = &idMap[_Symbol]{}

func NewSymbol(s string) _Symbol {
	return symbolManager.NameToId(s)
}

func genSym() _Symbol {
	return NewSymbol(fmt.Sprintf("-gensym-%d-", symbolManager.Count()))
}

func funGensym(ctx context.Context, w *World) (Node, error) {
	return genSym(), nil
}

var symbolClass = registerNewBuiltInClass[Symbol]("<symbol>")

type Reserved int

func (r Reserved) Id() int {
	return int(r)
}

func (_ Reserved) ClassOf() Class {
	return symbolClass
}

func (r Reserved) Eval(_ context.Context, w *World) (Node, error) {
	return w.Get(r)
}

func (r Reserved) Equals(n Node, m EqlMode) bool {
	ns, ok := n.(Reserved)
	return ok && r == ns
}

func (r Reserved) String() string {
	return reservedManager.IdToName(r)
}

var reservedManager = &idMap[Reserved]{}

func NewReserved(name string) Reserved {
	return reservedManager.NameToId(name)
}
