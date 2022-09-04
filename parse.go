package gmnlisp

import (
	"fmt"
	"io"
	"regexp"
	"strconv"
	"strings"
	"unicode/utf8"
)

var rxFloat = regexp.MustCompile(`^-?[0-9]+\.[0-9]*$`)

var rxInteger = regexp.MustCompile(`^-?[0-9]+$`)

var (
	dotSymbol        = NewSymbol(".")
	parenCloseSymbol = NewSymbol(")")
	quoteSymbol      = NewSymbol("quote")
	functionSymbol   = NewSymbol("function")
)

func nodes2cons(nodes []Node) Node {
	if nodes == nil || len(nodes) <= 0 {
		return Null
	}
	var cons Node = Null
	if len(nodes) >= 3 && nodes[len(nodes)-2] == dotSymbol {
		// dot pairs
		cons = nodes[len(nodes)-1]
		for i := len(nodes) - 3; i >= 0; i-- {
			cons = &Cons{
				Car: nodes[i],
				Cdr: cons,
			}
		}
	} else {
		for i := len(nodes) - 1; i >= 0; i-- {
			cons = &Cons{
				Car: nodes[i],
				Cdr: cons,
			}
		}
	}
	return cons
}

var escapeSequenceReplacer = strings.NewReplacer(
	"\\r", "\r",
	"\\n", "\n",
	"\\t", "\t",
	"\\b", "\b",
	"\\\"", "\"",
)

func newQuote(value Node) Node {
	return &Cons{Car: quoteSymbol, Cdr: &Cons{Car: value, Cdr: Null}}
}

func ReadNode(rs io.RuneScanner) (Node, error) {
	token, err := readToken(rs)
	if err != nil {
		return nil, err
	}
	if token == "'" {
		quoted, err := ReadNode(rs)
		if err != nil {
			if err == io.EOF {
				return nil, ErrTooShortTokens
			}
			return nil, err
		}
		return newQuote(quoted), nil
	}
	if token == "#'" {
		function, err := ReadNode(rs)
		if err != nil {
			if err == io.EOF {
				return nil, ErrTooShortTokens
			}
			return nil, err
		}
		return &Cons{Car: functionSymbol, Cdr: &Cons{Car: function, Cdr: Null}}, nil
	}
	if token == "(" {
		nodes := []Node{}
		for {
			if err != nil {
				if err == io.EOF {
					return nil, ErrTooShortTokens
				}
				return nil, err
			}
			var node1 Node
			node1, err = ReadNode(rs)
			if err != nil {
				if err == io.EOF {
					return nil, ErrTooShortTokens
				}
				return nil, err
			}
			if node1 == parenCloseSymbol {
				return nodes2cons(nodes), nil
			}
			nodes = append(nodes, node1)
		}
	}
	if len(token) > 0 && token[0] == ':' {
		return Keyword(token), nil
	}
	if rxFloat.MatchString(token) {
		val, err := strconv.ParseFloat(token, 64)
		if err != nil {
			return nil, fmt.Errorf("%s: %w", token, err)
		}
		return Float(val), nil
	}
	if rxInteger.MatchString(token) {
		val, err := strconv.ParseInt(token, 10, 63)
		if err != nil {
			return nil, fmt.Errorf("%s: %w", token, err)
		}
		return Integer(val), nil
	}
	if strings.HasPrefix(token, "#\\") {
		var val rune
		switch token[2:] {
		case "tab":
			val = '\t'
		case "linefeed", "newline":
			val = '\n'
		case "return":
			val = '\r'
		case "space":
			val = ' '
		default:
			val, _ = utf8.DecodeRuneInString(token[2:])
		}
		return Rune(val), nil
	}
	if len(token) > 0 && token[0] == '"' {
		token = token[1:]
		if L := len(token); L > 0 && token[L-1] == '"' {
			token = token[:L-1]
		}
		token = escapeSequenceReplacer.Replace(token)
		// UTF32String or UTF8String
		return String(token), nil
	}
	if token == "nil" {
		return Null, nil
	}
	return NewSymbol(token), nil
}

func ReadAll(rs io.RuneScanner) ([]Node, error) {
	result := []Node{}
	for {
		token, err := ReadNode(rs)
		if err != nil {
			if err == io.EOF {
				return result, nil
			}
			return nil, err
		}
		result = append(result, token)
	}
	return result, nil
}
