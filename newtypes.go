package gmnlisp

import (
	"fmt"
	"io"
)

var streamClass = registerClass(&BuiltInClass{
	name: NewSymbol("<stream>"),
	instanceP: func(value Node) bool {
		if _, ok := value.(io.Reader); ok {
			return true
		}
		_, ok := value.(io.Writer)
		return ok
	},
	create: func() Node {
		return &StringBuilder{}
	},
})

func (t *_Macro) Equals(Node, EqlMode) bool {
	return false
}

func (t *_Macro) String() string {
	return fmt.Sprintf("<macro>: %p", t)
}
