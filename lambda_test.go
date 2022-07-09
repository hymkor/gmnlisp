package gmnlisp

import (
	"testing"
)

func TestProgn(t *testing.T) {
	testOp(t,`(progn 1)`, NodeInteger(1))
	testOp(t,`(progn 1 2)`, NodeInteger(2))
}
