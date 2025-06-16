package subst

import (
	"context"

	. "github.com/hymkor/gmnlisp"
)

func init() {
	Export(NewSymbol("subst"), &Function{C: 3, F: funSubst})
}

func subst(newItem, oldItem, list Node) Node {
	if list.Equals(oldItem, STRICT) {
		return newItem
	}
	cons, ok := list.(*Cons)
	if ok {
		return &Cons{
			Car: subst(newItem, oldItem, cons.Car),
			Cdr: subst(newItem, oldItem, cons.Cdr),
		}
	}
	return list
}

// funSubst implements (subst NEWITEM OLDITEM LIST)
func funSubst(_ context.Context, _ *World, argv []Node) (Node, error) {
	return subst(argv[0], argv[1], argv[2]), nil
}
