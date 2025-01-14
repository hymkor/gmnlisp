package gmnlisp

import (
	"bufio"
	"fmt"
	"io"
	"os"
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

var stringBuilderClass = &_BuiltInClass{
	name: NewSymbol("<string-builder>"),
	instanceP: func(value Node) bool {
		_, ok := value.(*StringBuilder)
		return ok
	},
	create: func() Node {
		return &StringBuilder{}
	},
	super: []Class{objectClass, streamClass},
}

func (*StringBuilder) ClassOf() Class {
	return stringBuilderClass
}

func (t *StringBuilder) Equals(other Node, _ EqlMode) bool {
	o, ok := other.(*StringBuilder)
	if !ok {
		return false
	}
	return t.String() == o.String()
}

var inputStreamClass = &_BuiltInClass{
	name: NewSymbol("<input-stream>"),
	instanceP: func(value Node) bool {
		_, ok := value.(*inputStream)
		return ok
	},
	create: func() Node {
		return &inputStream{
			_Reader:  bufio.NewReader(os.Stdin),
			file:     os.Stdin,
			isClosed: false,
		}
	},
	super: []Class{objectClass, streamClass},
}

func (t *inputStream) ClassOf() Class {
	return inputStreamClass
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

func (t *_OutputFileStream) Equals(other Node, _ EqlMode) bool {
	o, ok := other.(*_OutputFileStream)
	if !ok {
		return false
	}
	return t.file.Fd() == o.file.Fd()
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
