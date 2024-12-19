package gmnlisp

import (
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

func (t *inputStream) Equals(Node, EqlMode) bool {
	return false
}

func (t *inputStream) String() string {
	return "(*inputStream)"
}

func (*_OutputFileStream) ClassOf() Class {
	return streamClass
}

func (t *_OutputFileStream) Equals(Node, EqlMode) bool {
	return false
}

func (t *_OutputFileStream) String() string {
	return "(*_OutputFileStream)"
}

func (t *_Macro) Equals(Node, EqlMode) bool {
	return false
}

func (t *_Macro) String() string {
	return "(*_Macro)"
}

func (_ReaderNode) ClassOf() Class {
	return streamClass
}

func (t _ReaderNode) Equals(Node, EqlMode) bool {
	return false
}

func (t _ReaderNode) String() string {
	return "(_ReaderNode)"
}

var writerNodeClass = registerNewBuiltInClass[_WriterNode]("<_WriterNode>")

func (_WriterNode) ClassOf() Class {
	return streamClass
}

func (t _WriterNode) Equals(Node, EqlMode) bool {
	return false
}

func (t _WriterNode) String() string {
	return "(_WriterNode)"
}
