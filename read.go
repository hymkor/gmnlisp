package gmnlisp

import (
	"bytes"
	"fmt"
	"io"
	"regexp"
	"strconv"
	"strings"
)

var rxFloat = regexp.MustCompile(`^-?[0-9]+\.[0-9]*$`)

var rxInteger = regexp.MustCompile(`^-?[0-9]+$`)

func nodes2cons(nodes []Node) Node {
	if nodes == nil {
		return Null
	}
	switch len(nodes) {
	case 0:
		return Null
	case 1:
		return &Cons{nodes[0], Null}
	case 3:
		if sym, ok := nodes[1].(Symbol); ok && string(sym) == "." {
			return &Cons{nodes[0], nodes[2]}
		}
		fallthrough
	default:
		return &Cons{Car: nodes[0], Cdr: nodes2cons(nodes[1:])}
	}
}

func readNode(tokenGetter func() (string, error)) (Node, error) {
	token, err := tokenGetter()
	if err != nil {
		return nil, err
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
			node1, err = readNode(tokenGetter)
			if err != nil {
				return nil, err
			}
			if node1 == Symbol(")") {
				return nodes2cons(nodes), nil
			}
			nodes = append(nodes, node1)
		}
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
	if strings.HasPrefix(token, "\"") {
		return String(strings.Replace(token, "\"", "", -1)), nil
	}
	if token == "nil" {
		return Null, nil
	}
	return Symbol(token), nil
}

type Slice []Node

func (ns Slice) Eval(w *World) (Node, error) {
	var result Node = Null
	var err error

	for _, c := range ns {
		result, err = c.Eval(w)
		if err != nil {
			return result, err
		}
	}
	return result, nil
}

func Read(r io.RuneReader) (Slice, error) {
	tokenGetter := newTokenizer(r)
	result := []Node{}
	for {
		token, err := readNode(tokenGetter)
		if err != nil {
			if err == io.EOF {
				return result, nil
			}
			return nil, err
		}
		token, err = macroQuote(token)
		if err != nil {
			return nil, err
		}
		result = append(result, token)
	}
	return Slice(result), nil
}

func ReadString(s string) (Slice, error) {
	return Read(strings.NewReader(s))
}

func ReadBytes(bin []byte) (Slice, error) {
	return Read(bytes.NewReader(bin))
}
