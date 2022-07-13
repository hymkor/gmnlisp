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

func readTokens(sc *_TokenScanner) (Node, error) {
	token := sc.Token()
	sc.Scan()
	if token == "(" {
		nodes := []Node{}
		for {
			if sc.EOF {
				return nil, ErrTooShortTokens
			}
			if sc.Token() == ")" {
				sc.Scan()
				return nodes2cons(nodes), nil
			}
			node1, err := readTokens(sc)
			if err != nil {
				return nil, err
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

type NodeSlice []Node

func (ns NodeSlice) Eval(w *World) (Node, error) {
	var result Node
	var err error

	for _, c := range ns {
		result, err = c.Eval(w)
		if err != nil {
			return result, err
		}
	}
	return result, nil
}

func Read(r io.Reader) (NodeSlice, error) {
	sc := newTokenScanner(r)
	if !sc.Scan() {
		return nil, nil
	}
	result := []Node{}
	for !sc.EOF {
		token, err := readTokens(sc)
		if err != nil {
			return nil, err
		}
		token, err = macroQuote(token)
		if err != nil {
			return nil, err
		}
		result = append(result, token)
	}
	return NodeSlice(result), nil
}

func ReadString(s string) (NodeSlice, error) {
	return Read(strings.NewReader(s))
}

func ReadBytes(bin []byte) (NodeSlice, error) {
	return Read(bytes.NewReader(bin))
}
