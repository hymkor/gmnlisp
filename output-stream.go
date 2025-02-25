package gmnlisp

import (
	"bufio"
	"bytes"
	"fmt"
	"io"
	"os"
)

type _WriterNode struct {
	_Writer io.Writer
	column  int
}

var writerNodeClass = registerNewBuiltInClass[_WriterNode]("<writer>")

func (w *_WriterNode) Column() int {
	return w.column
}

func (w *_WriterNode) Write(p []byte) (int, error) {
	n, err := w._Writer.Write(p)
	if n >= 1 {
		if pos := bytes.LastIndexByte(p[:n], '\n'); pos >= 0 {
			w.column = n - pos - 1
		} else {
			w.column += n
		}
	}
	return n, err
}

func (_WriterNode) ClassOf() Class {
	return streamClass
}

func (t _WriterNode) Equals(Node, EqlMode) bool {
	return false
}

func (t _WriterNode) String() string {
	return fmt.Sprintf("<writer>: %p", &t)
}

type outputStream struct {
	w        *bufio.Writer
	file     *os.File
	column   int
	isClosed bool
}

func newOutputFileStream(f *os.File) *outputStream {
	return &outputStream{
		w:    bufio.NewWriter(f),
		file: f,
	}
}

func (o *outputStream) Column() int {
	return o.column
}

func (o *outputStream) Write(p []byte) (int, error) {
	n, err := o.w.Write(p)
	if n >= 1 {
		if pos := bytes.LastIndexByte(p[:n], '\n'); pos >= 0 {
			o.column = n - pos - 1
		} else {
			o.column += n
		}
	}
	return n, err
}

func (o *outputStream) IsClosed() bool {
	return o.isClosed
}

func (o *outputStream) Flush() {
	o.w.Flush()
	o.file.Sync()
}

func (o *outputStream) Close() error {
	o.w.Flush()
	o.isClosed = true
	return o.file.Close()
}

func (o *outputStream) FilePosition() (int64, error) {
	o.w.Flush()
	return o.file.Seek(0, os.SEEK_CUR)
}

func (o *outputStream) SetFilePosition(n int64) (int64, error) {
	o.w.Flush()
	return o.file.Seek(n, os.SEEK_SET)
}

func (*outputStream) ClassOf() Class {
	return outputStreamClass
}

func (t *outputStream) Equals(other Node, _ EqlMode) bool {
	o, ok := other.(*outputStream)
	if !ok {
		return false
	}
	return t.file.Fd() == o.file.Fd()
}

func (t *outputStream) String() string {
	return fmt.Sprintf("<output-stream>: %p", t)
}
