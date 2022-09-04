package auto

import (
	"context"
	"strings"

	. "github.com/hymkor/gmnlisp"
)

func Using(w *World) *World {
	return w.Let(Variables{
		NewSymbol("strcase"): &Function{C: 1, F: funStrCase},
	})
}

func funStrCase(ctx context.Context, w *World, argv []Node) (Node, error) {
	// from autolisp
	if str, ok := argv[0].(UTF32String); ok {
		return UTF32String(strings.ToUpper(string(str))), nil
	} else if str, ok := argv[0].(UTF8String); ok {
		return UTF8String(strings.ToUpper(string(str))), nil
	}
	return nil, ErrExpectedString
}
