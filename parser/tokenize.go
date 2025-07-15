package parser

import (
	"io"
	"regexp"
	"strings"
	"unicode"
)

func skipComment(r io.RuneScanner) (bool, error) {
	c, _, err := r.ReadRune()
	if err != nil {
		return false, err
	}
	if c != '|' {
		if e := r.UnreadRune(); e != nil {
			panic(e.Error())
		}
		return false, nil
	}
	nest := 1
	for {
		c, _, err = r.ReadRune()
		if err != nil {
			return true, err
		}
		switch c {
		case '|':
			c, _, err = r.ReadRune()
			if err != nil {
				return true, err
			}
			if c == '#' {
				nest--
				if nest == 0 {
					return true, nil
				}
			} else {
				r.UnreadRune()
			}
		case '#':
			c, _, err = r.ReadRune()
			if err != nil {
				return true, err
			}
			if c == '|' {
				nest++
			} else {
				if e := r.UnreadRune(); e != nil {
					panic(e.Error())
				}
			}
		}
	}
}

var rxSharpAndNumber = regexp.MustCompile(`^#(\d+a)?$`)

func readtokenWord(r io.RuneScanner) (string, error) {
	var buffer strings.Builder

	quote := false
	bar4symbol := false
	lastLastRune := '\u0000'
	for {
		lastRune, _, err := r.ReadRune()
		if err != nil {
			return buffer.String(), err
		}

		if lastRune == '\\' {
			nextRune, _, err := r.ReadRune()
			if err != nil {
				return "", io.ErrUnexpectedEOF
			}
			if nextRune == '\\' {
				buffer.WriteString(`\\`)
				continue
			} else if nextRune == '"' {
				buffer.WriteString(`\"`)
				continue
			} else {
				r.UnreadRune()
			}
		}

		if !quote && !bar4symbol {
			if lastRune == '#' {
				done, err := skipComment(r)
				if err != nil {
					return "", err
				}
				if done {
					continue
				}
			}
			if lastRune == '(' && rxSharpAndNumber.MatchString(buffer.String()) {
				buffer.WriteRune(lastRune)
				return buffer.String(), nil
			}
			if lastRune == ')' || lastRune == '(' || lastRune == ';' || unicode.IsSpace(lastRune) {
				r.UnreadRune()
				return buffer.String(), nil
			}
		}
		if lastRune == '"' {
			quote = !quote
		}
		if !quote && lastRune == '|' && lastLastRune != '\\' {
			bar4symbol = !bar4symbol
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
		if strings.ContainsRune("',`()", lastRune) {
			token := string(lastRune)
			return token, nil
		}
		if e := r.UnreadRune(); e != nil {
			panic(e.Error())
		}
		var token string
		token, err = readtokenWord(r)
		if token != "" {
			return token, nil
		}
	}
}
