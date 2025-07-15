package gmnlisp

import (
	"io"
	"math/big"

	"github.com/hymkor/gmnlisp/parser"
)

var (
	ampRest         = NewKeyword("&rest")
	commaSymbol     = NewSymbol(",")
	symUnquote      = NewSymbol("unquote")
	nulSymbol       = NewSymbol("")
	quoteSymbol     = NewReserved("quote")
	backQuoteSymbol = NewSymbol("quasiquote")
	slashSymbol     = NewSymbol("/")
	colonRest       = NewKeyword(":rest")
)

var parser1 = &parser.Parser[Node]{
	Cons:    func(car, cdr Node) Node { return &Cons{Car: car, Cdr: cdr} },
	Int:     func(n int64) Node { return Integer(n) },
	BigInt:  func(n *big.Int) Node { return BigInt{Int: n} },
	Float:   func(f float64) Node { return Float(f) },
	String:  func(s string) Node { return String(s) },
	Array:   func(list []Node, dim []int) Node { return &Array{list: list, dim: dim} },
	Keyword: func(s string) Node { return NewKeyword(s) },
	Rune:    func(r rune) Node { return Rune(r) },
	Symbol: func(s string) Node {
		if r, ok := reservedManager.find(s); ok {
			return r
		}
		return NewSymbol(s)
	},
	Null: func() Node { return Null },
	True: func() Node { return True },
}

func ReadNode(rs io.RuneScanner) (Node, error) {
	return parser1.Read(rs)
}

func tryParseAsNumber(token string) (Node, bool, error) {
	return parser1.TryParseAsNumber(token)
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
