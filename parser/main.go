// Package parser provides a generic S-expression parser for ISLisp-like languages.
// It reads input from an io.RuneScanner and constructs Lisp objects via
// user-supplied constructor functions.

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
	rxFloat1     = regexp.MustCompile(`^[\-\+]?[0-9]+\.[0-9]+([eE][\-\+]?\d+)?$`)
	rxFloat2     = regexp.MustCompile(`^[\-\+]?[0-9]+[eE][-+]?\d+$`)
	rxInteger    = regexp.MustCompile(`^[\-\+]?[0-9]+$`)
	rxHexInteger = regexp.MustCompile(`^#[Xx][\+\-]?[0-9A-Fa-f]+$`)
	rxOctInteger = regexp.MustCompile(`^#[Oo][\+\-]?[0-7]+$`)
	rxBinInteger = regexp.MustCompile(`^#[Bb][\+\-]?[01]+$`)
	rxArray      = regexp.MustCompile(`^#(\d*)[aA]\(`)
	rx0Array     = regexp.MustCompile(`^#0[aA]`)
)

var (
	// ErrTooFewArguments is returned when a list or array has fewer elements than expected.
	ErrTooFewArguments = errors.New("too few arguments")

	// ErrTooManyArguments is returned when a list or array has more elements than expected.
	ErrTooManyArguments = errors.New("too many arguments")

	// ErrTooShortTokens is returned when the input ends unexpectedly (e.g., an unmatched open parenthesis).
	ErrTooShortTokens = errors.New("too short tokens")
)

// Parser represents a generic S-expression parser.
// The type parameter N corresponds to the target object type used in the host Lisp implementation.
// Users must provide constructor functions for various Lisp data types (e.g., Int, Symbol, Cons).

type Parser[N comparable] struct {
	Cons    func(N, N) N       // Cons constructs a cons cell from two objects.
	Int     func(int64) N      // Int constructs an integer object from an int64 value.
	BigInt  func(*big.Int) N   // BigInt constructs an integer object from a *big.Int value.
	Float   func(float64) N    // Float constructs a floating-point object from a float64 value.
	String  func(string) N     // String constructs a string object from a Go string.
	Symbol  func(string) N     // Symbol constructs a symbol object from a Go string.
	Array   func([]N, []int) N // Array constructs an array object from a flat list of elements and their dimensions.
	Keyword func(string) N     // Keyword constructs a keyword object from a Go string.
	Rune    func(rune) N       // Rune constructs a character object from a rune.
	Null    func() N           // Null returns the Lisp nil object.
	True    func() N           // True returns the Lisp true object.

	dotSymbol        N
	functionSymbol   N
	parenCloseSymbol N
	quoteSymbol      N
	quasiquoteSymbol N
	unquoteSymbol    N

	initialized bool
}

func (p *Parser[N]) nodes2cons(nodes []N) N {
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

func (p *Parser[N]) readArray(lenDim int, rs io.RuneScanner) (N, error) {
	nodes := []N{}
	dim := make([]int, lenDim)
	countDim := 0
	fix := make([]bool, lenDim)
	for {
		if countDim == lenDim-1 {
			newNode, err := p.readNode(rs)
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
	}
}

func (p *Parser[N]) newQuote(value N) N {
	return p.Cons(p.quoteSymbol, p.Cons(value, p.Null()))
}

func (p *Parser[N]) newBackQuote(value N) N {
	return p.Cons(p.quasiquoteSymbol, p.Cons(value, p.Null()))
}

func (p *Parser[N]) tryParseAsFloat(token string) (N, bool, error) {
	if !rxFloat1.MatchString(token) && !rxFloat2.MatchString(token) {
		return p.Null(), false, nil
	}
	val, err := strconv.ParseFloat(token, 64)
	if err != nil {
		return p.Null(), true, err
	}
	return p.Float(val), true, nil
}

func (p *Parser[N]) tryParseAsInt(token string) (N, bool, error) {
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

func (p *Parser[N]) tryParseAsNumber(token string) (N, bool, error) {
	if val, ok, err := p.tryParseAsInt(token); ok {
		if err != nil {
			return p.Null(), true, err
		}
		return val, true, nil
	}
	if val, ok, err := p.tryParseAsFloat(token); ok {
		if err != nil {
			return p.Null(), true, err
		}
		return val, true, nil
	}
	return p.Null(), false, nil
}

func (p *Parser[N]) readUntilCloseParen(rs io.RuneScanner) ([]N, error) {
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
		node1, err = p.readNode(rs)
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

func (p *Parser[N]) readNode(rs io.RuneScanner) (N, error) {
	token, err := readToken(rs)
	if err != nil {
		return p.Null(), err
	}
	if token == "`" {
		quoted, err := p.readNode(rs)
		if err != nil {
			if err == io.EOF {
				return p.Null(), ErrTooShortTokens
			}
			return p.Null(), err
		}
		return p.newBackQuote(quoted), nil
	}
	if token == "'" {
		quoted, err := p.readNode(rs)
		if err != nil {
			if err == io.EOF {
				return p.Null(), ErrTooShortTokens
			}
			return p.Null(), err
		}
		return p.newQuote(quoted), nil
	}
	if token == "," {
		quoted, err := p.readNode(rs)
		if err != nil {
			if err == io.EOF {
				return p.Null(), ErrTooShortTokens
			}
			return p.Null(), err
		}
		return p.Cons(p.unquoteSymbol, p.Cons(quoted, p.Null())), nil
	}
	if token == "#'" {
		function, err := p.readNode(rs)
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
		val, err := p.readNode(strings.NewReader(token[3:]))
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
		backSlash := false
		carriageReturn := false
		for _, c := range token {
			if carriageReturn {
				if c != '\n' {
					buffer.WriteByte('\r')
				}
				carriageReturn = false
			}
			if backSlash {
				switch c {
				case '\\':
					buffer.WriteByte('\\')
				case '"':
					buffer.WriteByte('"')
				default:
					buffer.WriteRune(c)
				}
				backSlash = false
			} else if c == '\\' {
				backSlash = true
			} else if c == '\r' {
				carriageReturn = true
			} else {
				buffer.WriteRune(c)
			}
		}
		if carriageReturn {
			buffer.WriteByte('\r')
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

func (p *Parser[N]) init() {
	if !p.initialized {
		p.initialized = true

		p.dotSymbol = p.Symbol(".")
		p.functionSymbol = p.Symbol("function")
		p.parenCloseSymbol = p.Symbol(")")
		p.quoteSymbol = p.Symbol("quote")
		p.quasiquoteSymbol = p.Symbol("quasiquote")
		p.unquoteSymbol = p.Symbol("unquote")

		if p.Cons == nil {
			panic("Parser.Cons is not set")
		}
		if p.Int == nil {
			panic("Parser.Int is not set")
		}
		if p.BigInt == nil {
			panic("Parser.BigInt is not set")
		}
		if p.Float == nil {
			panic("Parser.Float is not set")
		}
		if p.String == nil {
			panic("Parser.String is not set")
		}
		if p.Array == nil {
			panic("Parser.Array is not set")
		}
		if p.Keyword == nil {
			panic("Parser.Keyword is not set")
		}
		if p.Rune == nil {
			panic("Parser.Rune is not set")
		}
		if p.Null == nil {
			panic("Parser.Null is not set")
		}
		if p.True == nil {
			panic("Parser.True is not set")
		}

	}
}

// Read parses a single Lisp object from the given io.RuneScanner.
// It returns the resulting object of type N, or an error if the input is malformed.
func (p *Parser[N]) Read(rs io.RuneScanner) (N, error) {
	p.init()
	return p.readNode(rs)
}

// TryParseAsNumber attempts to parse a token as a numeric value.
// If successful, it returns a numeric object created via the Int, BigInt, or Float constructor,
// along with true.
// If the token does not match a numeric format, it returns the result of Null() and false.
// An error is returned only when numeric parsing fails due to an internal format error.
func (p *Parser[N]) TryParseAsNumber(token string) (N, bool, error) {
	p.init()
	return p.tryParseAsNumber(token)
}
