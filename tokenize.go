package gmnlisp

import (
	"bytes"
	"io"
	"strings"
	"unicode"
)

func readtoken(r io.RuneReader, lastRune rune) (string, rune, error) {
	var buffer bytes.Buffer
	var err error

	quote := false
	lastLastRune := '\u0000'
	for {
		if !quote {
			if lastRune == ')' || lastRune == '(' || lastRune == ';' || unicode.IsSpace(lastRune) {
				return buffer.String(), lastRune, nil
			}
		}
		if lastLastRune != '\\' && lastRune == '"' {
			quote = !quote
		}
		buffer.WriteRune(lastRune)

		if !quote && lastLastRune == '#' && lastRune == '\'' {
			lastRune, _, err = r.ReadRune()
			return buffer.String(), lastRune, err
		}
		lastLastRune = lastRune
		lastRune, _, err = r.ReadRune()
		if err != nil {
			return buffer.String(), lastRune, err
		}
	}
}

func newTokenizer(r io.RuneReader) func() (string, error) {
	lastRune, _, err := r.ReadRune()
	if err == nil && (lastRune == '#' || lastRune == '@') {
		for {
			lastRune, _, err = r.ReadRune()
			if err != nil || lastRune == '\n' {
				break
			}
		}
	}
	return func() (string, error) {
		for {
			if err != nil {
				return "", err
			}
			if unicode.IsSpace(lastRune) {
				lastRune, _, err = r.ReadRune()
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
				lastRune, _, err = r.ReadRune()
				return token, nil
			}
			var token string
			token, lastRune, err = readtoken(r, lastRune)
			if token != "" {
				return token, nil
			}
		}
	}
}
