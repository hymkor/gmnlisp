package gmnlisp

import (
	"bytes"
	"io"
	"strings"
	"text/scanner"
	"unicode"
)

func readtoken(r *scanner.Scanner, rune1 rune) (string, rune) {
	var buf1 bytes.Buffer
	quote := false
	for rune1 != scanner.EOF {
		if !quote {
			if rune1 == ')' || rune1 == '(' || unicode.IsSpace(rune1) {
				break
			}
		}
		if rune1 == '"' {
			quote = !quote
		}
		buf1.WriteRune(rune1)
		rune1 = r.Next()
	}
	return buf1.String(), rune1
}

type TokenScanner struct {
	lastToken string
	lastRune  rune
	sc        scanner.Scanner
}

func NewTokenScanner(r io.Reader) *TokenScanner {
	tr := &TokenScanner{}
	tr.sc.Init(r)
	tr.lastRune = tr.sc.Next()
	return tr
}

func (tr *TokenScanner) Text() string {
	return tr.lastToken
}

func (tr *TokenScanner) Scan() bool {
	for {
		if tr.lastRune == scanner.EOF {
			return false
		}
		for unicode.IsSpace(tr.lastRune) {
			tr.lastRune = tr.sc.Next()
			continue
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

func StringToTokens(s string) (tokens []string) {
	sc := NewTokenScanner(strings.NewReader(s))
	for sc.Scan() {
		tokens = append(tokens, sc.Text())
	}
	return tokens
}
