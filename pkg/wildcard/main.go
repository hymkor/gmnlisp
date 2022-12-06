package wildcard

import (
	"context"
	"fmt"
	"path/filepath"

	"github.com/hymkor/gmnlisp"
)

var Declare = &gmnlisp.Function{F: funWildcard}

func funWildcard(_ context.Context, w *gmnlisp.World, list []gmnlisp.Node) (gmnlisp.Node, error) {
	pattern, ok := list[0].(gmnlisp.String)
	if !ok {
		return nil, fmt.Errorf("%w: %#v", gmnlisp.ErrExpectedString, list[0])
	}
	files, err := filepath.Glob(pattern.String())
	if err != nil {
		return nil, fmt.Errorf("%w: %#v", err, pattern)
	}
	var result gmnlisp.Node
	for i := len(files) - 1; i >= 0; i-- {
		result = &gmnlisp.Cons{
			Car: gmnlisp.String(files[i]),
			Cdr: result,
		}
	}
	return result, nil
}
