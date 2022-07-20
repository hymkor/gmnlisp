package gmnlisp

import (
	"strings"
	"testing"
)

func TestTokenizer1(t *testing.T) {
	tzer := newTokenizer(strings.NewReader("1 2"))
	token, err := tzer()
	if token != "1" || err != nil {
		t.Fatal("1")
	}
	token, err = tzer()
	if token != "2" || err != nil {
		t.Fatal("2")
	}
	token, err = tzer()
	if err == nil {
		t.Fatal("3")
	}
	token, err = tzer()
	if err == nil {
		t.Fatal("4")
	}

	tzer = newTokenizer(strings.NewReader(""))
	token, err = tzer()
	if err == nil {
		t.Fatal("empty string")
	}

	tzer = newTokenizer(strings.NewReader(" "))
	token, err = tzer()
	if err == nil {
		t.Fatal("empty string")
	}

	assertEqual(t, `"\""`, String(`"`))
}

func TestShebang(t *testing.T) {
	assertEqual(t, `@gmnlisp.exe "%~f0" & exit /b
		(+ 1 2)`, Integer(3))

	assertEqual(t, `#!gmnlisp
		(+ 3 4)`, Integer(7))

}
