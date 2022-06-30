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
				return nil, nil, errors.New("too short tokens")
			}
			if tokens[0] == ")" {
				return nodes2cons(nodes), tokens[1:], nil
			}
			var err error
			var node1 Node
			node1, tokens, err = readTokens(tokens)
			if err != nil {
				return nil, nil, err
			}
			nodes = append(nodes, node1)
		}
	}
	if RxNumber.MatchString(tokens[0]) {
		val, err := strconv.ParseInt(tokens[0], 10, 63)
		if err != nil {
			return nil, nil, fmt.Errorf("%s: %w", tokens[0], err)
		}
		return NodeInteger(val), tokens[1:], nil
	}
	if strings.HasPrefix(tokens[0], "\"") {
		return NodeString(strings.Replace(tokens[0], "\"", "", -1)), tokens[1:], nil
	}
	return NodeSymbol(tokens[0]), tokens[1:], nil
}

func ReadTokens(tokens []string) Node {
	list, _, _ := readTokens(tokens)
	return list
}

func ReadString(s string) Node {
	return ReadTokens(StringToTokens(s))
}
