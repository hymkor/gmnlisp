package gommon

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

func Tokenize(r io.Reader) []string {
	var scr1 scanner.Scanner
	scr1.Init(r)

	list1 := make([]string, 0, 100)
	for rune1 := scr1.Next(); rune1 != scanner.EOF; {
		for rune1 != scanner.EOF && unicode.IsSpace(rune1) {
			rune1 = scr1.Next()
		}
		if strings.ContainsRune("'()", rune1) {
			list1 = append(list1, string(rune1))
			rune1 = scr1.Next()
		} else {
			var token string
			token, rune1 = readtoken(&scr1, rune1)
			if token != "" {
				list1 = append(list1, token)
			}
		}
	}
	return list1
}

func StringToTokens(s string) []string {
	return Tokenize(strings.NewReader(s))
}
