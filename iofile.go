package gmnlisp

import (
	"bufio"
	"bytes"
	"context"
	"io"
	"os"
)

type IOFile struct {
	reader       *bufio.Reader
	writer       *bufio.Writer
	file         *os.File
	isClosed     bool
	column       int
	elementClass int64
}

func (iof *IOFile) ElementClass() int64 {
	return iof.elementClass
}

var _ _Reader = &IOFile{}

func (iof *IOFile) Read(b []byte) (int, error) {
	iof.writer.Flush()
	return iof.reader.Read(b)
}

func (iof *IOFile) ReadByte() (byte, error) {
	iof.writer.Flush()
	return iof.reader.ReadByte()
}

func (iof *IOFile) ReadRune() (rune, int, error) {
	iof.writer.Flush()
	return iof.reader.ReadRune()
}

func (iof *IOFile) UnreadRune() error {
	return iof.reader.UnreadRune()
}

func (iof *IOFile) Close() error {
	iof.writer.Flush()
	iof.isClosed = true
	return iof.file.Close()
}

func (iof *IOFile) IsClosed() bool {
	return iof.isClosed
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

func (t *IOFile) ClassOf() Class {
	return streamClass
}

func (t *IOFile) Equals(Node, EqlMode) bool {
	return false
}

func (t *IOFile) String() string {
	return "(*inputOutputStream)"
}

func (i *IOFile) FilePosition() (int64, error) {
	i.writer.Flush()
	z, err := i.file.Seek(0, io.SeekCurrent)
	if err != nil {
		return 0, err
	}
	return z - int64(i.reader.Buffered()), nil
}

func (i *IOFile) SetFilePosition(n int64) (int64, error) {
	i.writer.Flush()
	ret, err := i.file.Seek(n, io.SeekStart)
	i.reader.Reset(i.file)
	return ret, err
}

func (i *IOFile) QueryStreamReady() (Node, error) {
	if i.isClosed {
		return Null, StreamError{Stream: i}
	}
	i.writer.Flush()
	if _, err := i.file.Seek(0, io.SeekCurrent); err != nil {
		return Null, nil
	}
	return True, nil
}

func newIoFile(fname string, ec int64) (*IOFile, error) {
	f, err := os.OpenFile(fname, os.O_RDWR|os.O_CREATE, 0644)
	if err != nil {
		return nil, err
	}
	return &IOFile{
		reader:       bufio.NewReader(f),
		writer:       bufio.NewWriter(f),
		file:         f,
		elementClass: ec,
	}, nil
}

func funOpenIoFile(ctx context.Context, w *World, args []Node) (Node, error) {
	fname, err := ExpectClass[String](ctx, w, args[0])
	if err != nil {
		return nil, err
	}
	var elementClass int64
	if len(args) >= 2 {
		if elementClass, err = expectElementClass(ctx, w, args[1]); err != nil {
			return nil, err
		}
	}
	return newIoFile(string(fname), elementClass)
}

func cmdWithOpenIoFile(ctx context.Context, w *World, list Node) (Node, error) {
	param, list, err := Shift(list)
	if err != nil {
		return nil, err
	}
	varName, param, err := Shift(param)
	if err != nil {
		return nil, err
	}
	symbol, err := ExpectSymbol(ctx, w, varName)
	if err != nil {
		return nil, err
	}
	filenameNode, param, err := w.ShiftAndEvalCar(ctx, param)
	if err != nil {
		return nil, err
	}
	filename, err := ExpectClass[String](ctx, w, filenameNode)
	if err != nil {
		return nil, err
	}
	var elementClass int64 = 0
	if IsSome(param) {
		elementClassNode, _, err := w.ShiftAndEvalCar(ctx, param)
		if err != nil {
			return nil, err
		}
		if ec, ok := elementClassNode.(Integer); ok {
			elementClass = int64(ec)
		}
	}
	stream, err := newIoFile(string(filename), elementClass)
	if err != nil {
		return nil, err
	}
	defer stream.Close()

	nw := w.Let(&Pair{Key: symbol, Value: stream})
	return Progn(ctx, nw, list)
}
