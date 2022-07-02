package gommon

import (
	"errors"
	"fmt"
	"regexp"
	"strconv"
	"strings"
)

var RxNumber = regexp.MustCompile("^[0-9]+$")

func nodes2cons(nodes []Node) Node {
	switch len(nodes) {
	case 0:
		return &Null{}
	case 1:
		return &Cons{nodes[0], &Null{}}
	case 3:
		if sym, ok := nodes[1].(NodeSymbol); ok && string(sym) == "." {
			return &Cons{nodes[0], nodes[2]}
		}
		fallthrough
	default:
		return &Cons{Car: nodes[0], Cdr: nodes2cons(nodes[1:])}
	}
}

func readTokens(tokens []string) (Node, []string, error) {
	if len(tokens) < 1 {
		return &Null{}, tokens, nil
	}
	if tokens[0] == "(" {
		nodes := []Node{}
		tokens = tokens[1:]
		for {
			if len(tokens) < 1 {
				return nil, tokens, errors.New("too short tokens")
			}
			if tokens[0] == ")" {
				return nodes2cons(nodes), tokens[1:], nil
			}
			var err error
			var node1 Node
			node1, tokens, err = readTokens(tokens)
			if err != nil {
				return nil, tokens, err
			}
			nodes = append(nodes, node1)
		}
	}
	if RxNumber.MatchString(tokens[0]) {
		val, err := strconv.ParseInt(tokens[0], 10, 63)
		if err != nil {
			return nil, tokens, fmt.Errorf("%s: %w", tokens[0], err)
		}
		return NodeInteger(val), tokens[1:], nil
	}
	if strings.HasPrefix(tokens[0], "\"") {
		return NodeString(strings.Replace(tokens[0], "\"", "", -1)), tokens[1:], nil
	}
	if tokens[0] == "nil" {
		return &Null{}, tokens[1:], nil
	}
	return NodeSymbol(tokens[0]), tokens[1:], nil
}

func ReadString(s string) ([]Node, error) {
	tokens := StringToTokens(s)
	result := []Node{}
	for len(tokens) > 0 {
		var err error
		var list Node
		list, tokens, err = readTokens(tokens)
		if err != nil {
			return nil, err
		}
		result = append(result, list)
	}
	return result, nil
}
