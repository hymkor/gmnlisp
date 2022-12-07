package regexp

import (
	"context"
	"regexp"

	. "github.com/hymkor/gmnlisp"
)

func init() {
	AutoLoad(NewSymbol("=~"), &Function{C: 2, F: funFindAllStringSubmatch})
	AutoLoad(NewSymbol("=~i"), &Function{C: 2, F: funFindAllStringSubmatchIndex})
}

var regexpCache = map[string]*regexp.Regexp{}

func getRegexpParam(list []Node) (*regexp.Regexp, string, error) {
	_pattern, ok := list[0].(String)
	if !ok {
		return nil, "", MakeError(ErrExpectedString, list[0])
	}
	pattern := _pattern.String()
	reg, ok := regexpCache[pattern]
	if !ok {
		var err error
		reg, err = regexp.Compile(pattern)
		if err != nil {
			return nil, "", MakeError(err, pattern)
		}
	}
	str, ok := list[1].(String)
	if !ok {
		return nil, "", MakeError(ErrExpectedString, list[1])
	}
	return reg, str.String(), nil
}

func funFindAllStringSubmatch(ctx context.Context, w *World, list []Node) (Node, error) {
	reg, str, err := getRegexpParam(list)
	if err != nil {
		return nil, err
	}
	m := reg.FindAllStringSubmatch(str, -1)
	if m == nil {
		return Null, nil
	}
	var cons Node = Null
	for i := len(m) - 1; i >= 0; i-- {
		var sub Node = Null
		for j := len(m[i]) - 1; j >= 0; j-- {
			sub = &Cons{
				Car: String(m[i][j]),
				Cdr: sub,
			}
		}
		cons = &Cons{
			Car: sub,
			Cdr: cons,
		}
	}
	return cons, nil
}

func funFindAllStringSubmatchIndex(ctx context.Context, w *World, list []Node) (Node, error) {
	reg, str, err := getRegexpParam(list)
	if err != nil {
		return nil, err
	}
	m := reg.FindAllStringSubmatchIndex(str, -1)
	if m == nil {
		return Null, nil
	}
	var cons Node = Null
	for i := len(m) - 1; i >= 0; i-- {
		var sub Node = Null
		for j := len(m[i]) - 1; j >= 0; j-- {
			sub = &Cons{
				Car: Integer(m[i][j]),
				Cdr: sub,
			}
		}
		cons = &Cons{
			Car: sub,
			Cdr: cons,
		}
	}
	return cons, nil
}
