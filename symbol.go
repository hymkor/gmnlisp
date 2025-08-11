package gmnlisp

import (
	"context"
	"fmt"
	"strings"
	"sync"
)

type nameIDRegistry[T ~int] struct {
	mu      sync.RWMutex
	id2name [][2]string
	name2id map[string]T
}

func (idm *nameIDRegistry[T]) find(name string) (T, bool) {
	idm.mu.RLock()
	defer idm.mu.RUnlock()

	if idm.name2id == nil {
		var zero T
		return zero, false
	}
	name = strings.ToUpper(name)
	s, ok := idm.name2id[name]
	return s, ok
}

func (idm *nameIDRegistry[T]) NameToId(name string) T {
	upperName := strings.ToUpper(name)

	idm.mu.Lock()
	defer idm.mu.Unlock()

	if idm.name2id == nil {
		idm.name2id = make(map[string]T)
	}
	if id, ok := idm.name2id[upperName]; ok {
		return id
	}

	id := T(len(idm.name2id))
	idm.name2id[upperName] = id
	idm.id2name = append(idm.id2name, [...]string{upperName, name})
	return id
}

func (idm *nameIDRegistry[T]) Count() int {
	idm.mu.RLock()
	defer idm.mu.RUnlock()
	return len(idm.name2id)
}

func (idm *nameIDRegistry[T]) IdToName(id T) [2]string {
	idm.mu.RLock()
	defer idm.mu.RUnlock()

	if id < 0 || int(id) >= len(idm.id2name) {
		return [...]string{"(UNDEFINED)", "(undefined)"}
	}
	return idm.id2name[id]
}

type Symbol interface {
	Id() int
	Node
	OriginalString() string
}

type _Symbol int

func (s _Symbol) Id() int {
	return int(s)
}

func (_Symbol) ClassOf() Class {
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
	return symbolManager.IdToName(s)[0]
}

func (s _Symbol) OriginalString() string {
	return symbolManager.IdToName(s)[1]
}

var symbolManager = &nameIDRegistry[_Symbol]{}

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

func (Reserved) ClassOf() Class {
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
	return reservedManager.IdToName(r)[0]
}

func (s Reserved) OriginalString() string {
	return reservedManager.IdToName(s)[1]
}

var reservedManager = &nameIDRegistry[Reserved]{}

func NewReserved(name string) Reserved {
	return reservedManager.NameToId(name)
}
