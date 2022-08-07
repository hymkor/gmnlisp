package gmnlisp

import (
	"context"
	"fmt"
	"io"
	"strings"
	"unicode"
)

type _TrueType struct{}

func (_TrueType) PrintTo(w io.Writer, m PrintMode) {
	io.WriteString(w, "T")
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

func (_NullType) PrintTo(w io.Writer, m PrintMode) {
	io.WriteString(w, "nil")
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

type String []Rune

var unescapeSequenceReplacer = strings.NewReplacer(
	"\n", "\\n",
	"\r", "\\r",
	"\t", "\\t",
	"\b", "\\b",
	"\"", "\\\"",
)

func (s String) PrintTo(w io.Writer, m PrintMode) {
	if m == PRINC {
		io.WriteString(w, string(s))
	} else {
		fmt.Fprintf(w, `"%s"`, unescapeSequenceReplacer.Replace(string(s)))
	}
}

func (s String) Eval(context.Context, *World) (Node, error) {
	return s, nil // errors.New("String can not be evaluate.")
}

func (s String) Equals(n Node, m EqlMode) bool {
	ns, ok := n.(String)
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

// firstAndRest returns first character, rest string and true.
// When string is empty, boolean is false.
func (s String) firstAndRest() (Node, Node, bool, func(Node) error) {
	if len(s) <= 0 {
		return nil, Null, false, nil
	}
	return Rune(s[0]), String(s[1:]), true, func(value Node) error {
		r, ok := value.(Rune)
		if !ok {
			return ErrExpectedCharacter
		}
		s[0] = r
		return nil
	}
}

func (s String) Add(n Node) (Node, error) {
	if value, ok := n.(String); ok {
		return String(append(s, value...)), nil
	}
	return nil, fmt.Errorf("%w: `%s`", ErrNotSupportType, toString(n, PRINT))
}

func (s String) LessThan(n Node) (bool, error) {
	if ns, ok := n.(String); ok {
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

var emptyString String

type Symbol string

func (s Symbol) PrintTo(w io.Writer, m PrintMode) {
	io.WriteString(w, string(s))
}

func (s Symbol) Eval(_ context.Context, w *World) (Node, error) {
	return w.Get(s)
}

func (s Symbol) Equals(n Node, m EqlMode) bool {
	ns, ok := n.(Symbol)
	if m == EQUALP {
		return ok && strings.EqualFold(string(s), string(ns))
	} else {
		return ok && s == ns
	}
}

type Rune rune

func (r Rune) PrintTo(w io.Writer, m PrintMode) {
	if m == PRINT {
		switch r {
		case '\t':
			io.WriteString(w, `#\tab`)
		case '\n':
			io.WriteString(w, `#\linefeed`)
		case '\r':
			io.WriteString(w, `#\return`)
		case ' ':
			io.WriteString(w, `#\space`)
		default:
			if unicode.IsLetter(rune(r)) {
				fmt.Fprintf(w, `#\%c`, rune(r))
			} else {
				fmt.Fprintf(w, `#\U%04X`, rune(r))
			}
		}
	} else {
		fmt.Fprintf(w, "%c", rune(r))
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
	if m == EQUAL {
		return false
	}
	if value, ok := n.(String); ok {
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

func (k Keyword) PrintTo(w io.Writer, m PrintMode) {
	io.WriteString(w, string(k))
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
