package gmnlisp

import (
	"testing"
)

func TestCmdCons(t *testing.T) {
	testOp(t, "(cons 1 2)", &Cons{Car: NodeInteger(1), Cdr: NodeInteger(2)})
	testOp(t, "(quote (1 . 2))", &Cons{Car: NodeInteger(1), Cdr: NodeInteger(2)})
}
