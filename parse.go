package gmnlisp

import (
	"errors"
	"fmt"
	"io"
	"regexp"
	"strconv"
	"strings"
	"unicode/utf8"
)

var (
	rxFloat   = regexp.MustCompile(`^-?[0-9]+\.[0-9]*$`)
	rxInteger = regexp.MustCompile(`^-?[0-9]+$`)
	rxArray   = regexp.MustCompile(`^#(\d*)a\(`)
)

var (
	dotSymbol        = NewSymbol(".")
	parenCloseSymbol = NewSymbol(")")
	quoteSymbol      = NewSymbol("quote")
	functionSymbol   = NewSymbol("function")
	listSymbol       = NewSymbol("list")
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

func readArrayDimN(dim int, rs io.RuneScanner) (Node, error) {
	if dim <= 1 {
		nodes, err := readUntilCloseParen(rs)
		if err != nil {
			return nil, err
		}
		return Vector(nodes), nil
	}
	nodes := []Node{}
	for {
		token, err := readToken(rs)
		if err != nil {
			return nil, err
		}
		if token == "(" {
			tmp, err := readArrayDimN(dim-1, rs)
			if err != nil {
				return nil, err
			}
			nodes = append(nodes, tmp)
		} else if token == ")" {
			return Vector(nodes), nil
		} else {
			return nil, errors.New("Expected array")
		}
	}
}

func newQuote(value Node) Node {
	return &Cons{Car: quoteSymbol, Cdr: &Cons{Car: value, Cdr: Null}}
}

func newBackQuote(value Node) Node {
	cons, ok := value.(*Cons)
	if !ok {
		return &Cons{Car: quoteSymbol, Cdr: &Cons{Car: value, Cdr: Null}}
	}
	list := []Node{listSymbol}
	comma := false
	for {
		if comma {
			list = append(list, cons.Car)
			comma = false
		} else if cons.Car == NewSymbol(",") {
			comma = true
		} else {
			list = append(list, newBackQuote(cons.Car))
		}
		if IsNull(cons.Cdr) {
			return List(list...)
		}
		if _cons, ok := cons.Cdr.(*Cons); ok {
			cons = _cons
		} else {
			var result Node = cons.Cdr
			for i := len(list) - 1; i >= 0; i-- {
				result = &Cons{Car: list[i], Cdr: result}
			}
			return result
		}
	}
}

func tryParseAsFloat(token string) (Node, bool, error) {
	if !rxFloat.MatchString(token) {
		return nil, false, nil
	}
	val, err := strconv.ParseFloat(token, 64)
	if err != nil {
		return nil, true, fmt.Errorf("%s: %w", token, err)
	}
	return Float(val), true, nil
}

func tryParseAsInt(token string) (Node, bool, error) {
	if !rxInteger.MatchString(token) {
		return nil, false, nil
	}
	val, err := strconv.ParseInt(token, 10, 63)
	if err != nil {
		return nil, true, fmt.Errorf("%s: %w", token, err)
	}
	return Integer(val), true, nil
}

func tryParseAsNumber(token string) (Node, bool, error) {
	if val, ok, err := tryParseAsFloat(token); ok {
		if err != nil {
			return nil, true, fmt.Errorf("%w (%s)", ErrCanNotParseNumber, err.Error())
		}
		return val, true, nil
	}
	if val, ok, err := tryParseAsInt(token); ok {
		if err != nil {
			return nil, true, fmt.Errorf("%w (%s)", ErrCanNotParseNumber, err.Error())
		}
		return val, true, nil
	}
	return nil, false, nil
}

func readUntilCloseParen(rs io.RuneScanner) ([]Node, error) {
	nodes := []Node{}
	var err error
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
			return nodes, nil
		}
		nodes = append(nodes, node1)
	}
}

func ReadNode(rs io.RuneScanner) (Node, error) {
	token, err := readToken(rs)
	if err != nil {
		return nil, err
	}
	if token == "`" {
		quoted, err := ReadNode(rs)
		if err != nil {
			if err == io.EOF {
				return nil, ErrTooShortTokens
			}
			return nil, err
		}
		return newBackQuote(quoted), nil
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
	if token == "#(" {
		nodes, err := readUntilCloseParen(rs)
		if err != nil {
			return nil, err
		}
		return Vector(nodes), nil
	}
	if m := rxArray.FindStringSubmatch(token); m != nil {
		dim, err := strconv.Atoi(m[1])
		if err != nil {
			panic(err.Error())
		}
		return readArrayDimN(dim, rs)
	}
	if token == "(" {
		nodes, err := readUntilCloseParen(rs)
		if err != nil {
			return nil, err
		}
		return nodes2cons(nodes), nil
	}
	if len(token) > 0 && token[0] == ':' {
		return Keyword(token), nil
	}
	if val, ok, err := tryParseAsNumber(token); ok {
		return val, err
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
		var buffer strings.Builder
		flag := false
		for _, c := range token {
			if flag {
				switch c {
				case '\\':
					buffer.WriteByte('\\')
				case '"':
					buffer.WriteByte('"')
				default:
					buffer.WriteRune(c)
				}
				flag = false
			} else if c == '\\' {
				flag = true
			} else {
				buffer.WriteRune(c)
			}
		}
		token = buffer.String()
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
