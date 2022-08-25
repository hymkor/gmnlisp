package gmnlisp

import (
	"bytes"
	"io"
	"strings"
	"unicode"
)

func readtoken(r io.RuneScanner) (string, error) {
	var buffer bytes.Buffer

	lastRune, _, err := r.ReadRune()
	if err != nil {
		return "", err
	}

	quote := false
	lastLastRune := '\u0000'
	for {
		if !quote {
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
		lastRune, _, err = r.ReadRune()
		if err != nil {
			return buffer.String(), err
		}
	}
}

func ReadToken(r io.RuneScanner) (string, error) {
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
		token, err = readtoken(r)
		if token != "" {
			return token, nil
		}
	}
}
