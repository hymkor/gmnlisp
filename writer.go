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

func (w *_WriterNode) Write(p []byte) (nn int, err error) {
	nn, err = w._Writer.Write(p)
	if nn >= 1 {
		if pos := bytes.LastIndexByte(p[:nn], '\n'); pos >= 0 {
			w.column = nn - pos
		} else {
			w.column += nn
		}
	}
	return nn, err
}

type _OutputFileStream struct {
	_WriterNode
	file     *os.File
	isClosed bool
}

func newOutputFileStream(f *os.File) *_OutputFileStream {
	return &_OutputFileStream{
		_WriterNode: _WriterNode{
			_Writer: bufio.NewWriter(f),
		},
		file: f,
	}
}

func (o *_OutputFileStream) IsClosed() bool {
	return o.isClosed
}

func (o *_OutputFileStream) Close() error {
	o._WriterNode._Writer.(*bufio.Writer).Flush()
	o.isClosed = true
	return o.file.Close()
}

func (o *_OutputFileStream) FilePosition() (int64, error) {
	o._WriterNode._Writer.(*bufio.Writer).Flush()
	return o.file.Seek(0, os.SEEK_CUR)
}

func (o *_OutputFileStream) SetFilePosition(n int64) (int64, error) {
	o._WriterNode._Writer.(*bufio.Writer).Flush()
	return o.file.Seek(n, os.SEEK_SET)
}
