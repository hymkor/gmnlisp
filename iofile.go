package gmnlisp

import (
	"bufio"
	"bytes"
	"context"
	"os"
)

type IOFile struct {
	inputStream
	writer *bufio.Writer
	column int
}

func (iof *IOFile) Close() error {
	iof.writer.Flush()
	return iof.inputStream.Close()
}

func (iof *IOFile) Column() int {
	return iof.column
}

func (iof *IOFile) Write(b []byte) (int, error) {
	n, err := iof.writer.Write(b)
	if pos := bytes.LastIndexByte(b[:n], '\n'); pos >= 0 {
		iof.column = n - pos
	} else {
		iof.column += n
	}
	return n, err
}

var inputOutputStreamClass = registerNewBuiltInClass[*IOFile]("<input-output-stream>")

func (t *IOFile) ClassOf() Class {
	return inputOutputStreamClass
}

func (t *IOFile) Equals(Node, EqlMode) bool {
	return false
}

func (t *IOFile) String() string {
	return "(*inputOutputStream)"
}

func newIoFile(fname string) (*IOFile, error) {
	f, err := os.OpenFile(fname, os.O_RDWR, 0644)
	if err != nil {
		return nil, err
	}
	return &IOFile{
		inputStream: inputStream{
			_Reader: bufio.NewReader(f),
			file:    f,
		},
		writer: bufio.NewWriter(f),
	}, nil
}

func funOpenIoFile(ctx context.Context, w *World, args []Node) (Node, error) {
	fname, err := ExpectClass[String](ctx, w, args[0])
	if err != nil {
		return nil, err
	}
	return newIoFile(string(fname))
}
