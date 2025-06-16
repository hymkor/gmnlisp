package parser

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
	_, err = readToken(rs)
	if err == nil {
		t.Fatal("3")
	}
	_, err = readToken(rs)
	if err == nil {
		t.Fatal("4")
	}

	rs = strings.NewReader("")
	_, err = readToken(rs)
	if err == nil {
		t.Fatal("empty string")
	}

	rs = strings.NewReader(" ")
	_, err = readToken(rs)
	if err == nil {
		t.Fatal("empty string")
	}
}
