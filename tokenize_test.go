package gmnlisp

import (
	"strings"
	"testing"
)

func TestTokenizer1(t *testing.T) {
	rs := strings.NewReader("1 2")
	token, err := readToken(rs)
	if token != "1" || err != nil {
		t.Fatal("1")
	}
	token, err = readToken(rs)
	if token != "2" || err != nil {
		t.Fatal("2")
	}
	token, err = readToken(rs)
	if err == nil {
		t.Fatal("3")
	}
	token, err = readToken(rs)
	if err == nil {
		t.Fatal("4")
	}

	rs = strings.NewReader("")
	token, err = readToken(rs)
	if err == nil {
		t.Fatal("empty string")
	}

	rs = strings.NewReader(" ")
	token, err = readToken(rs)
	if err == nil {
		t.Fatal("empty string")
	}

	assertEqual(t, `"\""`, String(`"`))
}

func TestComment(t *testing.T) {
	assertEqual(t, `(list 1 #|ahaha ihihi|# 2)`, List(Integer(1), Integer(2)))
}

/*
func TestShebang(t *testing.T) {
	assertEqual(t, `@gmnlisp.exe "%~f0" & exit /b
		(+ 1 2)`, Integer(3))

	assertEqual(t, `#!gmnlisp
		(+ 3 4)`, Integer(7))

}
*/
