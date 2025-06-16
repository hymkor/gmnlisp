package wildcard

import (
	"context"
	"fmt"
	"path/filepath"

	"github.com/hymkor/gmnlisp"
)

func init() {
	gmnlisp.Export(gmnlisp.NewSymbol("wildcard"), &gmnlisp.Function{F: funWildcard})
}

func funWildcard(ctx context.Context, w *gmnlisp.World, list []gmnlisp.Node) (gmnlisp.Node, error) {
	pattern, err := gmnlisp.ExpectClass[gmnlisp.String](ctx, w, list[0])
	if err != nil {
		return nil, err
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
