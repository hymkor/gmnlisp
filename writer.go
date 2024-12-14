package gmnlisp

import (
	"bufio"
	"io"
	"os"
	"unicode/utf8"
)

type _Writer = io.Writer

type _WriterNode struct {
	_Writer
	lastOutputIsNotLf bool
}

type CanKnowLastOutput interface {
	IsLastOutputLf() bool
}

func (w *_WriterNode) IsLastOutputLf() bool {
	return !w.lastOutputIsNotLf
}

func (w *_WriterNode) Write(p []byte) (nn int, err error) {
	nn, err = w._Writer.Write(p)
	if nn >= 1 {
		w.lastOutputIsNotLf = (p[nn-1] != '\n')
	}
	return nn, err
}

func (w *_WriterNode) WriteByte(c byte) error {
	w.lastOutputIsNotLf = (c != '\n')
	_, err := w._Writer.Write([]byte{c})
	return err
}

func (w *_WriterNode) WriteRune(c rune) (int, error) {
	var buffer [utf8.UTFMax]byte
	size := utf8.EncodeRune(buffer[:], c)
	w.lastOutputIsNotLf = (c != '\n')
	return w._Writer.Write(buffer[:size])
}

func (w *_WriterNode) WriteString(s string) (size int, err error) {
	n, err := io.WriteString(w._Writer, s)
	if n > 0 {
		w.lastOutputIsNotLf = (s[n-1] != '\n')
	}
	return n, err
}

type _OutputFileStream struct {
	_WriterNode
	file *os.File
}

func newOutputFileStream(f *os.File) *_OutputFileStream {
	return &_OutputFileStream{
		_WriterNode: _WriterNode{
			_Writer: bufio.NewWriter(f),
		},
		file: f,
	}
}

func (o *_OutputFileStream) Close() error {
	o._WriterNode._Writer.(*bufio.Writer).Flush()
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
