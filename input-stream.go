package gmnlisp

import (
	"bufio"
	"context"
	"fmt"
	"io"
	"os"
)

type inputStream struct {
	_Reader      *bufio.Reader
	file         *os.File
	isClosed     bool
	elementClass int64
}

func (i *inputStream) ElementClass() int64 {
	return i.elementClass
}

var inputStreamClass = &BuiltInClass{
	name: NewSymbol("<input-stream>"),
	instanceP: func(value Node) bool {
		_, ok := value.(*inputStream)
		return ok
	},
	create: func() Node {
		return &inputStream{
			_Reader:  bufio.NewReader(os.Stdin),
			file:     os.Stdin,
			isClosed: false,
		}
	},
	super: []Class{ObjectClass, streamClass},
}

func (t *inputStream) ClassOf() Class {
	return inputStreamClass
}

func (t *inputStream) Equals(other Node, _ EqlMode) bool {
	o, ok := other.(*inputStream)
	if !ok {
		return false
	}
	return t.file.Fd() == o.file.Fd()
}

func (t *inputStream) String() string {
	return fmt.Sprintf("(struct <input-stream> (isClosed %v) (elementClass %d))",
		t.isClosed, t.elementClass)
}

func (i *inputStream) Read(b []byte) (int, error) {
	return i._Reader.Read(b)
}

func (i *inputStream) ReadByte() (byte, error) {
	return i._Reader.ReadByte()
}

func (i *inputStream) ReadRune() (r rune, size int, err error) {
	return i._Reader.ReadRune()
}

func (i *inputStream) UnreadRune() error {
	return i._Reader.UnreadRune()
}

func (i *inputStream) Close() error {
	i.isClosed = true
	return i.file.Close()
}

func (i *inputStream) IsClosed() bool {
	return i.isClosed
}

func (i *inputStream) FilePosition() (int64, error) {
	z, err := i.file.Seek(0, io.SeekCurrent)
	if err != nil {
		return 0, err
	}
	return z - int64(i._Reader.Buffered()), nil
}

func (i *inputStream) SetFilePosition(n int64) (int64, error) {
	ret, err := i.file.Seek(n, io.SeekStart)
	i._Reader.Reset(i.file)
	return ret, err
}

func (i *inputStream) QueryStreamReady() (Node, error) {
	if i.isClosed {
		return Null, StreamError{Stream: i}
	}
	if _, err := i.file.Seek(0, io.SeekCurrent); err != nil {
		return Null, nil
	}
	return True, nil
}

func openInputFile(ctx context.Context, w *World, fname Node, elementClass int64) (*inputStream, error) {
	filename, err := ExpectClass[String](ctx, w, fname)
	if err != nil {
		return nil, err
	}
	reader, err := os.Open(filename.String())
	if err != nil {
		return nil, err
	}
	return &inputStream{_Reader: bufio.NewReader(reader), file: reader, elementClass: elementClass}, nil
}

func funOpenInputFile(ctx context.Context, w *World, args []Node) (Node, error) {
	var elementClass int64 = 0
	if len(args) >= 2 {
		if v, err := expectElementClass(ctx, w, args[1]); err != nil {
			return nil, err
		} else {
			elementClass = int64(v)
		}
	}
	return openInputFile(ctx, w, args[0], elementClass)
}

func cmdWithOpenInputFile(ctx context.Context, w *World, list Node) (Node, error) {
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
	filename, param, err := w.ShiftAndEvalCar(ctx, param)
	if err != nil {
		return nil, err
	}
	var elementClass int64 = 0
	if IsSome(param) {
		elementClassNode, _, err := w.ShiftAndEvalCar(ctx, param)
		if err != nil {
			return nil, err
		}
		if v, err := expectElementClass(ctx, w, elementClassNode); err != nil {
			return nil, err
		} else {
			elementClass = int64(v)
		}
	}
	stream, err := openInputFile(ctx, w, filename, elementClass)
	if err != nil {
		return nil, err
	}
	defer stream.Close()

	nw := w.Let(&Pair{Key: symbol, Value: stream})
	return Progn(ctx, nw, list)
}
