package gmnlisp

import (
	"testing"
)

func TestStringToToken(t *testing.T) {
	x := StringToTokens("a b c")
	if len(x) != 3 {
		t.Error("x-len\n")
	}
	if x[0] != "a" || x[1] != "b" || x[2] != "c" {
		t.FailNow()
	}

	y := StringToTokens("(a b c)")
	for i := 0; i < len(y); i++ {
		t.Logf("y[%d]=='%s'\n", i, y[i])
	}
	if len(y) != 5 {
		t.Errorf("y-len == %d\n", len(y))
	}
	if y[0] != "(" {
		t.Error("y[0]\n")
	}
	if y[1] != "a" {
		t.Error("y[1]\n")
	}
	if y[2] != "b" {
		t.Error("y[2]\n")
	}
	if y[3] != "c" {
		t.Error("y[3]\n")
	}
	if y[4] != ")" {
		t.Error("y[4]\n")
	}
}
