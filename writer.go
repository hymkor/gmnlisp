package gmnlisp

import (
	"bufio"
	"bytes"
	"io"
	"os"
)

type _WriterNode struct {
	_Writer io.Writer
	column  int
}

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
