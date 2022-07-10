package gmnlisp

import (
	"bytes"
	"io"
	"strings"
	"text/scanner"
	"unicode"
)

func readtoken(r *scanner.Scanner, lastRune rune) (string, rune) {
	var buf1 bytes.Buffer
	quote := false
	for lastRune != scanner.EOF {
		if !quote {
			if lastRune == ')' || lastRune == '(' || unicode.IsSpace(lastRune) {
				break
			}
		}
		if lastRune == '"' {
			quote = !quote
		}
		buf1.WriteRune(lastRune)
		lastRune = r.Next()
	}
	return buf1.String(), lastRune
}

type TokenScanner struct {
	lastToken string
	lastRune  rune
	sc        scanner.Scanner
	EOF       bool
}

func NewTokenScanner(r io.Reader) *TokenScanner {
	tr := &TokenScanner{}
	tr.sc.Init(r)
	tr.lastRune = tr.sc.Next()
	return tr
}

func (tr *TokenScanner) Token() string {
	return tr.lastToken
}

func (tr *TokenScanner) Scan() bool {
	for {
		if tr.lastRune == scanner.EOF {
			tr.EOF = true
			return false
		}
		for unicode.IsSpace(tr.lastRune) {
			tr.lastRune = tr.sc.Next()
			if tr.lastRune == scanner.EOF {
				tr.EOF = true
				return false
			}
		}
		if strings.ContainsRune("'()", tr.lastRune) {
			tr.lastToken = string(tr.lastRune)
			tr.lastRune = tr.sc.Next()
			return true
		}
		tr.lastToken, tr.lastRune = readtoken(&tr.sc, tr.lastRune)
		if tr.lastToken != "" {
			return true
		}
	}
}
