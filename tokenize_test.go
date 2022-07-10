package gmnlisp

import (
	"testing"
)

func TestStringToToken(t *testing.T) {
	x := StringToTokens("a b c")
	if len(x) != 3 {
		t.Fatal("x-len\n")
	}
	if x[0] != "a" || x[1] != "b" || x[2] != "c" {
		t.Fatal("`a b c` not `a` `b` `c`")
	}

	y := StringToTokens("(a b c)")
	if len(y) != 5 {
		t.Fatalf("y-len == %d\n", len(y))
	}
	if y[0] != "(" {
		t.Fatal("y[0]")
	}
	if y[1] != "a" {
		t.Fatal("y[1]")
	}
	if y[2] != "b" {
		t.Fatal("y[2]")
	}
	if y[3] != "c" {
		t.Fatal("y[3]")
	}
	if y[4] != ")" {
		t.Fatal("y[4]")
	}
}
