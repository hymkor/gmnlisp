package gmnlisp

import (
	"fmt"
	"io"
)

var streamClass = registerClass(&_BuiltInClass{
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

func (*StringBuilder) ClassOf() Class {
	return streamClass
}

func (t *StringBuilder) Equals(Node, EqlMode) bool {
	return false
}

func (t *inputStream) ClassOf() Class {
	return streamClass
}

func (t *inputStream) Equals(other Node, _ EqlMode) bool {
	o, ok := other.(*inputStream)
	if !ok {
		return false
	}
	return t.file.Fd() == o.file.Fd()
}

func (t *inputStream) String() string {
	return fmt.Sprintf("<input-stream>: %p", t)
}

func (*_OutputFileStream) ClassOf() Class {
	return streamClass
}

func (t *_OutputFileStream) Equals(Node, EqlMode) bool {
	return false
}

func (t *_OutputFileStream) String() string {
	return fmt.Sprintf("<output-stream>: %p", t)
}

func (t *_Macro) Equals(Node, EqlMode) bool {
	return false
}

func (t *_Macro) String() string {
	return fmt.Sprintf("<macro>: %p", t)
}

var writerNodeClass = registerNewBuiltInClass[_WriterNode]("<writer>")

func (_WriterNode) ClassOf() Class {
	return streamClass
}

func (t _WriterNode) Equals(Node, EqlMode) bool {
	return false
}

func (t _WriterNode) String() string {
	return fmt.Sprintf("<writer>: %p", &t)
}
