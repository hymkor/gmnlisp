package gmnlisp

import (
	"context"
	"io"
)

func (*StringBuilder) ClassOf() Class {
	return embedClassOf[*StringBuilder]("<string-builder>")
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

func (t *inputStream) ClassOf() Class {
	return embedClassOf[*inputStream]("<input-stream>")
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

func (*_OutputFileStream) ClassOf() Class {
	return embedClassOf[*_OutputFileStream]("<output-file-stream>")
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

func (_ReaderNode) ClassOf() Class {
	return embedClassOf[_ReaderNode]("<reader>")
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

func (_WriterNode) ClassOf() Class {
	return embedClassOf[_WriterNode]("<_WriterNode>")
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
