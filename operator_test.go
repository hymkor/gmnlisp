package gmnlisp

import (
	"testing"
)

func testOp[T Node](t *testing.T, equation string, expect T) {
	rv, err := Interpret(equation)
	if err != nil {
		t.Fatal(err.Error())
		return
	}
	value, ok := rv.(T)
	if !ok {
		t.Fatal("not a number")
		return
	}
	if !value.Equals(expect) {
		t.Fatalf("%s != %s (was %s)", equation, Node2String(expect), Node2String(value))
		return
	}
}

func TestOperator(t *testing.T) {
	testOp(t, "(+ 1 2)", NodeInteger(3))
	testOp(t, "(+ 1 2 3)", NodeInteger(6))
	testOp(t, "(- 10 9)", NodeInteger(1))
	testOp(t, "(- 10 1 2)", NodeInteger(7))
	testOp(t, "(* 1 2)", NodeInteger(2))
	testOp(t, "(* 1 2 3)", NodeInteger(6))
	testOp(t, "(/ 6 2)", NodeInteger(3))
	testOp(t, `(+ "1" "2")`, NodeString("12"))
}
