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

type _OutputFileStream struct {
	w        *bufio.Writer
	file     *os.File
	column   int
	isClosed bool
}

func newOutputFileStream(f *os.File) *_OutputFileStream {
	return &_OutputFileStream{
		w:    bufio.NewWriter(f),
		file: f,
	}
}

func (o *_OutputFileStream) Column() int {
	return o.column
}

func (o *_OutputFileStream) Write(p []byte) (int, error) {
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

func (o *_OutputFileStream) IsClosed() bool {
	return o.isClosed
}

func (o *_OutputFileStream) Flush() {
	o.w.Flush()
	o.file.Sync()
}

func (o *_OutputFileStream) Close() error {
	o.w.Flush()
	o.isClosed = true
	return o.file.Close()
}

func (o *_OutputFileStream) FilePosition() (int64, error) {
	o.w.Flush()
	return o.file.Seek(0, os.SEEK_CUR)
}

func (o *_OutputFileStream) SetFilePosition(n int64) (int64, error) {
	o.w.Flush()
	return o.file.Seek(n, os.SEEK_SET)
}
