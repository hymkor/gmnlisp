package gmnlisp

import (
	"context"
	"io"
)

var stringBuilderClass = registerNewBuiltInClass[*StringBuilder]("<string-builder>")

func (*StringBuilder) ClassOf() Class {
	return stringBuilderClass
}

func (t *StringBuilder) Eval(context.Context, *World) (Node, error) {
	return t, nil
}

func (t *StringBuilder) Equals(Node, EqlMode) bool {
	return false
}

func (t *StringBuilder) PrintTo(w io.Writer, m PrintMode) (int, error) {
	return io.WriteString(w, "(*StringBuilder)")
}

var inputStreamClass = registerNewBuiltInClass[*inputStream]("<input-stream>")

func (t *inputStream) ClassOf() Class {
	return inputStreamClass
}

func (t *inputStream) Eval(context.Context, *World) (Node, error) {
	return t, nil
}

func (t *inputStream) Equals(Node, EqlMode) bool {
	return false
}

func (t *inputStream) PrintTo(w io.Writer, m PrintMode) (int, error) {
	return io.WriteString(w, "(*inputStream)")
}

var outputFileStreamClass = registerNewBuiltInClass[*_OutputFileStream]("<output-file-stream>")

func (*_OutputFileStream) ClassOf() Class {
	return outputFileStreamClass
}

func (t *_OutputFileStream) Eval(context.Context, *World) (Node, error) {
	return t, nil
}

func (t *_OutputFileStream) Equals(Node, EqlMode) bool {
	return false
}

func (t *_OutputFileStream) PrintTo(w io.Writer, m PrintMode) (int, error) {
	return io.WriteString(w, "(*_OutputFileStream)")
}

func (t *_Macro) Eval(context.Context, *World) (Node, error) {
	return t, nil
}

func (t *_Macro) Equals(Node, EqlMode) bool {
	return false
}

func (t *_Macro) PrintTo(w io.Writer, m PrintMode) (int, error) {
	return io.WriteString(w, "(*_Macro)")
}

var readerNodeClass = registerNewBuiltInClass[_ReaderNode]("<reader>")

func (_ReaderNode) ClassOf() Class {
	return readerNodeClass
}

func (t _ReaderNode) Eval(context.Context, *World) (Node, error) {
	return t, nil
}

func (t _ReaderNode) Equals(Node, EqlMode) bool {
	return false
}

func (t _ReaderNode) PrintTo(w io.Writer, m PrintMode) (int, error) {
	return io.WriteString(w, "(_ReaderNode)")
}

var writerNodeClass = registerNewBuiltInClass[_WriterNode]("<_WriterNode>")

func (_WriterNode) ClassOf() Class {
	return writerNodeClass
}

func (t _WriterNode) Eval(context.Context, *World) (Node, error) {
	return t, nil
}

func (t _WriterNode) Equals(Node, EqlMode) bool {
	return false
}

func (t _WriterNode) PrintTo(w io.Writer, m PrintMode) (int, error) {
	return io.WriteString(w, "(_WriterNode)")
}
