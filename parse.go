package gmnlisp

import (
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
	rxArray   = regexp.MustCompile(`^#(\d*)[aA]\(`)
)

var (
	ampRest          = NewSymbol("&rest")
	commaSymbol      = NewSymbol(",")
	dotSymbol        = NewSymbol(".")
	functionSymbol   = NewSymbol("function")
	nulSymbol        = NewSymbol("")
	parenCloseSymbol = NewSymbol(")")
	quoteSymbol      = NewSymbol("quote")
	backQuoteSymbol  = NewSymbol("backquote")
	slashSymbol      = NewSymbol("/")
	colonRest        = NewKeyword(":rest")
)

func nodes2cons(nodes []Node) Node {
	if nodes == nil || len(nodes) <= 0 {
		return Null
	}
	var cons Node = Null
	if len(nodes) >= 3 && nodes[len(nodes)-2] == dotSymbol {
		// dot pairs
		cons = nodes[len(nodes)-1]
		nodes = nodes[:len(nodes)-2]
	}
	for i := len(nodes) - 1; i >= 0; i-- {
		cons = &Cons{
			Car: nodes[i],
			Cdr: cons,
		}
	}
	return cons
}

func readArray(lenDim int, rs io.RuneScanner) (Node, error) {
	nodes := []Node{}
	dim := make([]int, lenDim)
	countDim := 0
	fix := make([]bool, lenDim)
	for {
		if countDim == lenDim-1 {
			newNode, err := ReadNode(rs)
			if err != nil {
				return nil, err
			}
			if newNode == parenCloseSymbol {
				fix[countDim] = true
				countDim--
				if countDim < 0 {
					break
				}
			} else {
				nodes = append(nodes, newNode)
				if !fix[countDim] {
					dim[countDim]++
				}
			}
		} else {
			token, err := readToken(rs)
			if err != nil {
				return nil, err
			}
			if token == "(" {
				if !fix[countDim] {
					dim[countDim]++
				}
				countDim++
			} else if token == ")" {
				fix[countDim] = true
				countDim--
				if countDim < 0 {
					break
				}
			} else {
				return nil, fmt.Errorf("array syntax error: %#v", token)
			}
		}
	}
	size := 1
	for _, v := range dim {
		size *= v
	}
	if len(nodes) < size {
		return nil, ErrTooFewArguments
	} else if len(nodes) > size {
		return nil, ErrTooManyArguments
	} else {
		return &Array{
			list: nodes,
			dim:  dim,
		}, nil
	}
}

func newQuote(value Node) Node {
	return &Cons{Car: quoteSymbol, Cdr: &Cons{Car: value, Cdr: Null}}
}

func newBackQuote(value Node) Node {
	return &Cons{Car: backQuoteSymbol, Cdr: &Cons{Car: value, Cdr: Null}}
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
			return nil, true, fmt.Errorf("%w: (%s)", ErrCanNotParseNumber, err.Error())
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

var symbol4barReplacer = strings.NewReplacer(
	`\\`, `\`,
	`\|`, `|`)

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
		return readArray(1, rs)
	}
	if m := rxArray.FindStringSubmatch(token); m != nil {
		dim, err := strconv.Atoi(m[1])
		if err != nil {
			panic(err.Error())
		}
		return readArray(dim, rs)
	}
	if token == "(" {
		nodes, err := readUntilCloseParen(rs)
		if err != nil {
			return nil, err
		}
		return nodes2cons(nodes), nil
	}
	if len(token) > 0 && token[0] == ':' {
		return NewKeyword(token), nil
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
		return String(token), nil
	}
	if token[0] == '|' && token[len(token)-1] == '|' {
		return NewSymbol(symbol4barReplacer.Replace(token[1 : len(token)-1])), nil
	}
	if token[0] == ':' {
		return NewKeyword(token), nil
	}
	if strings.EqualFold(token, "t") {
		return True, nil
	}
	if strings.EqualFold(token, "nil") {
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
}
