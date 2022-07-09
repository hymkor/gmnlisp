package gmnlisp

import (
	"testing"
)

func TestCmdCond(t *testing.T) {
	testOp(t, `(cond (nil 1) (T 2))`, NodeInteger(2))
	testOp(t, `(cond ((equal 1 1) "a") ((equal 1 2) "b"))`, NodeString("a"))
}
