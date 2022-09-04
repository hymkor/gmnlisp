package gmnlisp

import (
	"context"
	"fmt"
	"io"
	"strings"
	"unicode"
	"unicode/utf8"
)

type _TrueType struct{}

func (_TrueType) PrintTo(w io.Writer, m PrintMode) (int, error) {
	return io.WriteString(w, "t")
}

func (t _TrueType) Eval(context.Context, *World) (Node, error) {
	return t, nil
}

var True Node = _TrueType{}

func (_TrueType) Equals(n Node, m EqlMode) bool {
	_, ok := n.(_TrueType)
	return ok
}

type _NullType struct{}

func (_NullType) PrintTo(w io.Writer, m PrintMode) (int, error) {
	return io.WriteString(w, "nil")
}

func (nt _NullType) Eval(context.Context, *World) (Node, error) {
	return nt, nil
}

func (nt _NullType) Equals(n Node, m EqlMode) bool {
	if n == nil {
		return true
	}
	_, ok := n.(_NullType)
	return ok
}

var Null Node = _NullType{}

type UTF32String []Rune
type UTF8String string

type String = UTF8String
type _StringBuilder = _UTF8StringBuilder

type StringTypes interface {
	Node
	String() string
	firstRuneAndRestString() (Rune, StringTypes, bool)
}

func (s UTF8String) String() string {
	return string(s)
}

func (s UTF8String) PrintTo(w io.Writer, m PrintMode) (int, error) {
	if m == PRINC {
		return io.WriteString(w, string(s))
	} else {
		return fmt.Fprintf(w, `"%s"`, unescapeSequenceReplacer.Replace(string(s)))
	}
}

func (s UTF8String) Eval(context.Context, *World) (Node, error) {
	return s, nil
}

func (s UTF8String) Equals(n Node, m EqlMode) bool {
	ns, ok := n.(UTF8String)
	if !ok {
		_ns, ok := n.(String)
		if !ok {
			return false
		}
		if m == STRICT {
			return false
		}
		ns = UTF8String(string(_ns))
	}
	if m == EQUALP {
		return strings.EqualFold(string(s), string(ns))
	}
	return string(s) == string(ns)
}

func (s UTF8String) firstRuneAndRestString() (Rune, StringTypes, bool) {
	if len(s) <= 0 {
		return Rune(utf8.RuneError), nil, false
	}
	r, siz := utf8.DecodeRuneInString(string(s))
	return Rune(r), UTF8String(s[siz:]), true
}

func (s UTF8String) firstAndRest() (Node, Node, bool, func(Node) error) {
	if len(s) <= 0 {
		return nil, Null, false, nil
	}
	r, siz := utf8.DecodeRuneInString(string(s))
	return Rune(r), UTF8String(s[siz:]), true, func(value Node) error {
		return ErrNotSupportType
	}
}

func (s UTF8String) Add(n Node) (Node, error) {
	if value, ok := n.(UTF8String); ok {
		news := make([]byte, 0, len(s)+len(value)+1)
		news = append(news, s...)
		news = append(news, value...)
		return UTF8String(news), nil
	}
	return nil, fmt.Errorf("%w: `%s`", ErrNotSupportType, toString(n, PRINT))
}

func (s UTF8String) LessThan(n Node) (bool, error) {
	ns, ok := n.(UTF8String)
	if !ok {
		return false, fmt.Errorf("%w: `%s`", ErrNotSupportType, toString(n, PRINT))
	}
	return string(s) < string(ns), nil
}

var unescapeSequenceReplacer = strings.NewReplacer(
	"\n", "\\n",
	"\r", "\\r",
	"\t", "\\t",
	"\b", "\\b",
	"\"", "\\\"",
)

func (s UTF32String) String() string {
	return string(s)
}

func (s UTF32String) PrintTo(w io.Writer, m PrintMode) (int, error) {
	if m == PRINC {
		return io.WriteString(w, string(s))
	} else {
		return fmt.Fprintf(w, `"%s"`, unescapeSequenceReplacer.Replace(string(s)))
	}
}

func (s UTF32String) Eval(context.Context, *World) (Node, error) {
	return s, nil // errors.New("UTF32String can not be evaluate.")
}

func (s UTF32String) Equals(n Node, m EqlMode) bool {
	ns, ok := n.(UTF32String)
	if !ok || len(s) != len(ns) {
		return false
	}
	if m == EQUALP {
		for i, c := range s {
			if c != ns[i] && unicode.ToLower(rune(c)) != unicode.ToLower(rune(ns[i])) {
				return false
			}
		}
		return true
	} else {
		for i, c := range s {
			if c != ns[i] {
				return false
			}
		}
		return true
	}
}

func (s UTF32String) firstRuneAndRestString() (Rune, StringTypes, bool) {
	if len(s) <= 0 {
		return Rune(utf8.RuneError), nil, false
	}
	return Rune(s[0]), UTF32String(s[1:]), true
}

// firstAndRest returns first character, rest string and true.
// When string is empty, boolean is false.
func (s UTF32String) firstAndRest() (Node, Node, bool, func(Node) error) {
	if len(s) <= 0 {
		return nil, Null, false, nil
	}
	return Rune(s[0]), UTF32String(s[1:]), true, func(value Node) error {
		r, ok := value.(Rune)
		if !ok {
			return ErrExpectedCharacter
		}
		s[0] = r
		return nil
	}
}

func (s UTF32String) Add(n Node) (Node, error) {
	if value, ok := n.(UTF32String); ok {
		return UTF32String(append(s, value...)), nil
	}
	return nil, fmt.Errorf("%w: `%s`", ErrNotSupportType, toString(n, PRINT))
}

func (s UTF32String) LessThan(n Node) (bool, error) {
	if ns, ok := n.(UTF32String); ok {
		equal := true
		for i, left := range s {
			if i >= len(ns) {
				return true, nil
			}
			right := ns[i]
			if left > right {
				return false, nil
			}
			if left < right {
				equal = false
			}
			if equal {
				return len(s) >= len(ns), nil
			}
			return true, nil
		}
		return true, nil
	}
	return false, fmt.Errorf("%w: `%s`", ErrNotSupportType, toString(n, PRINT))
}

func (s UTF32String) Aref(n int) (Node, func(Node) error, error) {
	if n < 0 || n >= len(s) {
		return nil, nil, ErrIndexOutOfRange
	}
	return Rune(s[n]), func(value Node) error {
		theRune, ok := value.(Rune)
		if !ok {
			return ErrNotSupportType
		}
		s[n] = theRune
		return nil
	}, nil
}

var emptyString UTF32String

type Symbol int

var symbols = []string{}

var symbolMap = map[string]Symbol{}

func NewSymbol(s string) Symbol {
	if value, ok := symbolMap[s]; ok {
		return value
	}
	value := Symbol(len(symbolMap))
	symbolMap[s] = value
	symbols = append(symbols, s)
	return value
}

func (s Symbol) PrintTo(w io.Writer, m PrintMode) (int, error) {
	return io.WriteString(w, symbols[s])
}

func (s Symbol) Eval(_ context.Context, w *World) (Node, error) {
	return w.Get(s)
}

func (s Symbol) Equals(n Node, m EqlMode) bool {
	ns, ok := n.(Symbol)
	return ok && s == ns
}

func (s Symbol) String() string {
	return symbols[s]
}

type Rune rune

func (r Rune) PrintTo(w io.Writer, m PrintMode) (int, error) {
	if m == PRINT {
		switch r {
		case '\t':
			return io.WriteString(w, `#\tab`)
		case '\n':
			return io.WriteString(w, `#\linefeed`)
		case '\r':
			return io.WriteString(w, `#\return`)
		case ' ':
			return io.WriteString(w, `#\space`)
		default:
			if unicode.IsLetter(rune(r)) {
				return fmt.Fprintf(w, `#\%c`, rune(r))
			} else {
				return fmt.Fprintf(w, `#\U%04X`, rune(r))
			}
		}
	} else {
		return fmt.Fprintf(w, "%c", rune(r))
	}
}

func (r Rune) Eval(_ context.Context, w *World) (Node, error) {
	return r, nil
}

func (r Rune) Equals(n Node, m EqlMode) bool {
	if value, ok := n.(Rune); ok {
		if r == value {
			return true
		}
		if m == EQUALP {
			return unicode.ToLower(rune(r)) == unicode.ToLower(rune(value))
		}
		return false
	}
	if m == STRICT {
		return false
	}
	if value, ok := n.(UTF32String); ok {
		return len(string(value)) == 1 && string(r) == string(value)
	}
	return false
}

func (r Rune) Add(n Node) (Node, error) {
	if value, ok := n.(Integer); ok {
		return r + Rune(value), nil
	}
	if value, ok := n.(Rune); ok {
		return r + value, nil
	}
	return nil, fmt.Errorf("%w: `%s`", ErrNotSupportType, toString(n, PRINT))
}

func (r Rune) Sub(n Node) (Node, error) {
	if value, ok := n.(Integer); ok {
		return r - Rune(value), nil
	}
	if value, ok := n.(Rune); ok {
		return r - value, nil
	}
	return nil, fmt.Errorf("%w: `%s`", ErrNotSupportType, toString(n, PRINT))
}

type Keyword string

func (k Keyword) PrintTo(w io.Writer, m PrintMode) (int, error) {
	return io.WriteString(w, string(k))
}

func (k Keyword) Eval(context.Context, *World) (Node, error) {
	return k, nil
}

func (k Keyword) Equals(n Node, m EqlMode) bool {
	if other, ok := n.(Keyword); ok {
		return strings.EqualFold(string(k), string(other))
	}
	return false
}
