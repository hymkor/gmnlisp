package parser

import (
	"errors"
	"fmt"
	"io"
	"math/big"
	"regexp"
	"strconv"
	"strings"
	"unicode/utf8"
)

var (
	rxFloat1     = regexp.MustCompile(`^[\-\+]?[0-9]+\.[0-9]*([eE][\-\+]?\d+)?$`)
	rxFloat2     = regexp.MustCompile(`^[\-\+]?[0-9]+[eE][-+]?\d+$`)
	rxInteger    = regexp.MustCompile(`^-?[0-9]+$`)
	rxHexInteger = regexp.MustCompile(`^\#[Xx][0-9A-Fa-f]+$`)
	rxOctInteger = regexp.MustCompile(`^\#[Oo][0-7]+$`)
	rxBinInteger = regexp.MustCompile(`^\#[Bb][01]+$`)
	rxArray      = regexp.MustCompile(`^#(\d*)[aA]\(`)
	rx0Array     = regexp.MustCompile(`^#0[aA]`)
)

var (
	ErrTooFewArguments   = errors.New("too few arguments")
	ErrTooManyArguments  = errors.New("too many arguments")
	ErrCanNotParseNumber = errors.New("can not parse number")
	ErrTooShortTokens    = errors.New("too short tokens")
)

type Factory[N comparable] interface {
	Cons(car, cdr N) N
	Int(int64) N
	BigInt(*big.Int) N
	Float(float64) N
	String(string) N
	Symbol(string) N
	Array([]N, []int) N
	Keyword(string) N
	Rune(rune) N
	Null() N
	True() N
}

type _Parser[N comparable] struct {
	Factory[N]

	dotSymbol        N
	functionSymbol   N
	parenCloseSymbol N
}

func (p *_Parser[N]) nodes2cons(nodes []N) N {
	if nodes == nil || len(nodes) <= 0 {
		return p.Null()
	}
	cons := p.Null()
	if len(nodes) >= 3 && nodes[len(nodes)-2] == p.dotSymbol {
		// dot pairs
		cons = nodes[len(nodes)-1]
		nodes = nodes[:len(nodes)-2]
	}
	for i := len(nodes) - 1; i >= 0; i-- {
		cons = p.Cons(nodes[i], cons)
	}
	return cons
}

func (p *_Parser[N]) readArray(lenDim int, rs io.RuneScanner) (N, error) {
	nodes := []N{}
	dim := make([]int, lenDim)
	countDim := 0
	fix := make([]bool, lenDim)
	for {
		if countDim == lenDim-1 {
			newNode, err := p.ReadNode(rs)
			if err != nil {
				return p.Null(), err
			}
			if newNode == p.parenCloseSymbol {
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
				return p.Null(), err
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
				return p.Null(), fmt.Errorf("array syntax error: %#v", token)
			}
		}
	}
	size := 1
	for _, v := range dim {
		size *= v
	}
	if len(nodes) < size {
		return p.Null(), ErrTooFewArguments
	} else if len(nodes) > size {
		return p.Null(), ErrTooManyArguments
	} else {
		return p.Array(nodes, dim), nil
		//&Array{
		//	list: nodes,
		//	dim:  dim,
		//}, nil
	}
}

func (p *_Parser[N]) newQuote(value N) N {
	return p.Cons(p.Symbol("quote"), p.Cons(value, p.Null()))
}

func (p *_Parser[N]) newBackQuote(value N) N {
	return p.Cons(p.Symbol("quasiquote"), p.Cons(value, p.Null()))
}

func (p *_Parser[N]) tryParseAsFloat(token string) (N, bool, error) {
	if !rxFloat1.MatchString(token) && !rxFloat2.MatchString(token) {
		return p.Null(), false, nil
	}
	val, err := strconv.ParseFloat(token, 64)
	if err != nil {
		return p.Null(), true, err
	}
	return p.Float(val), true, nil
}

func (p *_Parser[N]) tryParseAsInt(token string) (N, bool, error) {
	var val int64
	var err error
	var base = 10
	if rxInteger.MatchString(token) {
		val, err = strconv.ParseInt(token, 10, 64)
	} else if rxHexInteger.MatchString(token) {
		base = 16
		val, err = strconv.ParseInt(token[2:], 16, 64)
	} else if rxOctInteger.MatchString(token) {
		base = 8
		val, err = strconv.ParseInt(token[2:], 8, 64)
	} else if rxBinInteger.MatchString(token) {
		base = 2
		val, err = strconv.ParseInt(token[2:], 2, 64)
	} else {
		return p.Null(), false, nil
	}
	if err != nil {
		var numError *strconv.NumError
		if errors.As(err, &numError) {
			if errors.Is(numError.Err, strconv.ErrRange) {
				var v big.Int
				if _, ok := v.SetString(numError.Num, base); ok {
					return p.BigInt(&v), true, nil
				}
			}
		}
		return p.Null(), true, fmt.Errorf("%s: %w", token, err)
	}
	return p.Int(val), true, nil
}

func (p *_Parser[N]) tryParseAsNumber(token string) (N, bool, error) {
	if val, ok, err := p.tryParseAsFloat(token); ok {
		if err != nil {
			return p.Null(), true, fmt.Errorf("%w: (%s)", ErrCanNotParseNumber, err.Error())
		}
		return val, true, nil
	}
	if val, ok, err := p.tryParseAsInt(token); ok {
		if err != nil {
			return p.Null(), true, fmt.Errorf("%w (%s)", ErrCanNotParseNumber, err.Error())
		}
		return val, true, nil
	}
	return p.Null(), false, nil
}

func (p *_Parser[N]) readUntilCloseParen(rs io.RuneScanner) ([]N, error) {
	nodes := []N{}
	var err error
	for {
		if err != nil {
			if err == io.EOF {
				return nil, ErrTooShortTokens
			}
			return nil, err
		}
		var node1 N
		node1, err = p.ReadNode(rs)
		if err != nil {
			if err == io.EOF {
				return nil, ErrTooShortTokens
			}
			return nil, err
		}
		if node1 == p.parenCloseSymbol {
			return nodes, nil
		}
		nodes = append(nodes, node1)
	}
}

var symbol4barReplacer = strings.NewReplacer(
	`\\`, `\`,
	`\|`, `|`)

func (p *_Parser[N]) ReadNode(rs io.RuneScanner) (N, error) {
	token, err := readToken(rs)
	if err != nil {
		return p.Null(), err
	}
	if token == "`" {
		quoted, err := p.ReadNode(rs)
		if err != nil {
			if err == io.EOF {
				return p.Null(), ErrTooShortTokens
			}
			return p.Null(), err
		}
		return p.newBackQuote(quoted), nil
	}
	if token == "'" {
		quoted, err := p.ReadNode(rs)
		if err != nil {
			if err == io.EOF {
				return p.Null(), ErrTooShortTokens
			}
			return p.Null(), err
		}
		return p.newQuote(quoted), nil
	}
	if token == "," {
		quoted, err := p.ReadNode(rs)
		if err != nil {
			if err == io.EOF {
				return p.Null(), ErrTooShortTokens
			}
			return p.Null(), err
		}
		return p.Cons(p.Symbol("unquote"), p.Cons(quoted, p.Null())), nil
	}
	if token == "#'" {
		function, err := p.ReadNode(rs)
		if err != nil {
			if err == io.EOF {
				return p.Null(), ErrTooShortTokens
			}
			return p.Null(), err
		}
		return p.Cons(p.functionSymbol, p.Cons(function, p.Null())), nil
	}
	if token == "#(" {
		return p.readArray(1, rs)
	}
	if m := rxArray.FindStringSubmatch(token); m != nil {
		dim, err := strconv.Atoi(m[1])
		if err != nil {
			panic(err.Error())
		}
		return p.readArray(dim, rs)
	}
	if m := rx0Array.FindStringSubmatch(token); m != nil {
		val, err := p.ReadNode(strings.NewReader(token[3:]))
		if err != nil {
			return p.Null(), err
		}
		return p.Array([]N{val}, []int{}), nil
	}
	if token == "(" {
		nodes, err := p.readUntilCloseParen(rs)
		if err != nil {
			return p.Null(), err
		}
		return p.nodes2cons(nodes), nil
	}
	if len(token) > 0 && (token[0] == ':' || token[0] == '&') {
		return p.Keyword(token), nil
	}
	if val, ok, err := p.tryParseAsNumber(token); ok {
		return val, err
	}
	if strings.HasPrefix(token, "#\\") {
		var val rune
		switch strings.ToLower(token[2:]) {
		case "tab":
			val = '\t'
		case "linefeed", "newline":
			val = '\n'
		case "return":
			val = '\r'
		case "space":
			val = ' '
		default:
			if len(token) >= 3 && token[2] == 'U' && len(token[2:]) > 1 {
				if _val, err := strconv.ParseUint(token[3:], 16, 32); err == nil {
					val = rune(_val)
					break
				}
			}
			val, _ = utf8.DecodeRuneInString(token[2:])
		}
		return p.Rune(val), nil
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
		return p.String(token), nil
	}
	if token[0] == '|' && token[len(token)-1] == '|' {
		return p.Symbol(symbol4barReplacer.Replace(token[1 : len(token)-1])), nil
	}
	if token[0] == ':' {
		return p.Keyword(token), nil
	}
	if strings.EqualFold(token, "t") {
		return p.True(), nil
	}
	if strings.EqualFold(token, "nil") {
		return p.Null(), nil
	}
	return p.Symbol(token), nil
}

func newParser[N comparable](f Factory[N]) *_Parser[N] {
	return &_Parser[N]{
		Factory:          f,
		dotSymbol:        f.Symbol("."),
		functionSymbol:   f.Symbol("function"),
		parenCloseSymbol: f.Symbol(")"),
	}
}

func Read[N comparable](f Factory[N], rs io.RuneScanner) (N, error) {
	return newParser[N](f).ReadNode(rs)
}

func TryParseAsNumber[N comparable](f Factory[N], token string) (N, bool, error) {
	return newParser[N](f).tryParseAsNumber(token)
}

func NewQuote[N comparable](f Factory[N], value N) N {
	return newParser[N](f).newQuote(value)
}
