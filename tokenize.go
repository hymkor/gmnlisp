package gmnlisp

import (
	"io"
	"strings"
	"unicode"
)

func readtokenWord(r io.RuneScanner) (string, error) {
	var buffer strings.Builder

	quote := false
	lastLastRune := '\u0000'
	for {
		lastRune, _, err := r.ReadRune()
		if err != nil {
			return buffer.String(), err
		}

		if !quote {
			if lastLastRune == '#' && lastRune == '(' {
				buffer.WriteRune(lastRune)
				return buffer.String(), nil
			}
			if lastRune == ')' || lastRune == '(' || lastRune == ';' || unicode.IsSpace(lastRune) {
				r.UnreadRune()
				return buffer.String(), nil
			}
		}
		if lastLastRune != '\\' && lastRune == '"' {
			quote = !quote
		}
		buffer.WriteRune(lastRune)

		if !quote && lastLastRune == '#' && lastRune == '\'' {
			return buffer.String(), err
		}
		lastLastRune = lastRune
	}
}

func readToken(r io.RuneScanner) (string, error) {
	for {
		lastRune, _, err := r.ReadRune()
		if err != nil {
			return "", err
		}
		if unicode.IsSpace(lastRune) {
			continue
		}
		if lastRune == ';' {
			for err == nil && lastRune != '\n' {
				lastRune, _, err = r.ReadRune()
			}
			continue
		}
		if strings.ContainsRune("'()", lastRune) {
			token := string(lastRune)
			return token, nil
		}
		r.UnreadRune()
		var token string
		token, err = readtokenWord(r)
		if token != "" {
			return token, nil
		}
	}
}
