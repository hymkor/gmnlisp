package gmnlisp

import (
	"strings"
)

func (t ErrorNode) String() string {
	var buffer strings.Builder
	t.PrintTo(&buffer, PRINC)
	return buffer.String()
}

func (t ErrorNode) GoString() string {
	var buffer strings.Builder
	t.PrintTo(&buffer, PRINT)
	return buffer.String()
}

func (t Float) String() string {
	var buffer strings.Builder
	t.PrintTo(&buffer, PRINC)
	return buffer.String()
}

func (t Float) GoString() string {
	var buffer strings.Builder
	t.PrintTo(&buffer, PRINT)
	return buffer.String()
}

func (t Integer) String() string {
	var buffer strings.Builder
	t.PrintTo(&buffer, PRINC)
	return buffer.String()
}

func (t Integer) GoString() string {
	var buffer strings.Builder
	t.PrintTo(&buffer, PRINT)
	return buffer.String()
}

func (t _WriterNode) String() string {
	var buffer strings.Builder
	t.PrintTo(&buffer, PRINC)
	return buffer.String()
}

func (t _WriterNode) GoString() string {
	var buffer strings.Builder
	t.PrintTo(&buffer, PRINT)
	return buffer.String()
}

func (t _ReaderNode) String() string {
	var buffer strings.Builder
	t.PrintTo(&buffer, PRINC)
	return buffer.String()
}

func (t _ReaderNode) GoString() string {
	var buffer strings.Builder
	t.PrintTo(&buffer, PRINT)
	return buffer.String()
}

func (t _Macro) String() string {
	var buffer strings.Builder
	t.PrintTo(&buffer, PRINC)
	return buffer.String()
}

func (t _Macro) GoString() string {
	var buffer strings.Builder
	t.PrintTo(&buffer, PRINT)
	return buffer.String()
}

func (t _OutputFileStream) String() string {
	var buffer strings.Builder
	t.PrintTo(&buffer, PRINC)
	return buffer.String()
}

func (t _OutputFileStream) GoString() string {
	var buffer strings.Builder
	t.PrintTo(&buffer, PRINT)
	return buffer.String()
}

func (t inputStream) String() string {
	var buffer strings.Builder
	t.PrintTo(&buffer, PRINC)
	return buffer.String()
}

func (t inputStream) GoString() string {
	var buffer strings.Builder
	t.PrintTo(&buffer, PRINT)
	return buffer.String()
}

func (t _JoinedForm) String() string {
	var buffer strings.Builder
	t.PrintTo(&buffer, PRINC)
	return buffer.String()
}

func (t _JoinedForm) GoString() string {
	var buffer strings.Builder
	t.PrintTo(&buffer, PRINT)
	return buffer.String()
}

func (t LispString) String() string {
	var buffer strings.Builder
	t.PrintTo(&buffer, PRINC)
	return buffer.String()
}

func (t LispString) GoString() string {
	var buffer strings.Builder
	t.PrintTo(&buffer, PRINT)
	return buffer.String()
}

func (t SpecialF) String() string {
	var buffer strings.Builder
	t.PrintTo(&buffer, PRINC)
	return buffer.String()
}

func (t SpecialF) GoString() string {
	var buffer strings.Builder
	t.PrintTo(&buffer, PRINT)
	return buffer.String()
}

func (t _Lambda) String() string {
	var buffer strings.Builder
	t.PrintTo(&buffer, PRINC)
	return buffer.String()
}

func (t _Lambda) GoString() string {
	var buffer strings.Builder
	t.PrintTo(&buffer, PRINT)
	return buffer.String()
}

func (t _TrueType) String() string {
	var buffer strings.Builder
	t.PrintTo(&buffer, PRINC)
	return buffer.String()
}

func (t _TrueType) GoString() string {
	var buffer strings.Builder
	t.PrintTo(&buffer, PRINT)
	return buffer.String()
}

func (t Cons) String() string {
	var buffer strings.Builder
	t.PrintTo(&buffer, PRINC)
	return buffer.String()
}

func (t Cons) GoString() string {
	var buffer strings.Builder
	t.PrintTo(&buffer, PRINT)
	return buffer.String()
}

func (t Keyword) String() string {
	var buffer strings.Builder
	t.PrintTo(&buffer, PRINC)
	return buffer.String()
}

func (t Keyword) GoString() string {
	var buffer strings.Builder
	t.PrintTo(&buffer, PRINT)
	return buffer.String()
}

func (t Rune) String() string {
	var buffer strings.Builder
	t.PrintTo(&buffer, PRINC)
	return buffer.String()
}

func (t Rune) GoString() string {
	var buffer strings.Builder
	t.PrintTo(&buffer, PRINT)
	return buffer.String()
}

func (t _NullType) String() string {
	var buffer strings.Builder
	t.PrintTo(&buffer, PRINC)
	return buffer.String()
}

func (t _NullType) GoString() string {
	var buffer strings.Builder
	t.PrintTo(&buffer, PRINT)
	return buffer.String()
}

func (t Array) String() string {
	var buffer strings.Builder
	t.PrintTo(&buffer, PRINC)
	return buffer.String()
}

func (t Array) GoString() string {
	var buffer strings.Builder
	t.PrintTo(&buffer, PRINT)
	return buffer.String()
}

func (t Function) String() string {
	var buffer strings.Builder
	t.PrintTo(&buffer, PRINC)
	return buffer.String()
}

func (t Function) GoString() string {
	var buffer strings.Builder
	t.PrintTo(&buffer, PRINT)
	return buffer.String()
}

func (t _Hash) String() string {
	var buffer strings.Builder
	t.PrintTo(&buffer, PRINC)
	return buffer.String()
}

func (t _Hash) GoString() string {
	var buffer strings.Builder
	t.PrintTo(&buffer, PRINT)
	return buffer.String()
}
