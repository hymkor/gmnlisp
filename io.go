package gmnlisp

import (
	"bufio"
	"context"
	"errors"
	"fmt"
	"io"
	"os"
	"strconv"
	"strings"
)

func chomp(s string) string {
	L := len(s)
	if L > 0 && s[L-1] == '\n' {
		L--
		s = s[:L]
		if L > 0 && s[L-1] == '\r' {
			s = s[:L-1]
		}
	}
	return s
}

type _StreamInput struct {
	reader   _Reader
	eofFlag  bool
	eofValue Node
}

func newStreamInput(w *World, argv []Node) (*_StreamInput, error) {
	this := &_StreamInput{
		reader:   nil,
		eofFlag:  true,
		eofValue: Null,
	}
	switch len(argv) {
	default:
		return nil, ErrTooManyArguments
	case 3:
		this.eofValue = argv[2]
		fallthrough
	case 2:
		this.eofFlag = IsSome(argv[1])
		fallthrough
	case 1:
		var ok bool
		this.reader, ok = argv[0].(_Reader)
		if !ok {
			return nil, fmt.Errorf("expected Reader %#v", argv[0])
		}
	case 0:
		this.reader = w.Stdin()
	}
	return this, nil
}

func readString(r io.ByteReader, delim byte) (string, error) {
	var buffer strings.Builder
	for {
		b, err := r.ReadByte()
		if err != nil {
			return buffer.String(), err
		}
		buffer.WriteByte(b)
		if b == delim {
			return buffer.String(), nil
		}
	}
}

func funReadLine(_ context.Context, w *World, argv []Node) (Node, error) {
	stream, err := newStreamInput(w, argv)
	if err != nil {
		return nil, err
	}
	type _ReadStringer interface {
		ReadString(delim byte) (string, error)
	}
	var s string
	if r, ok := stream.reader.(_ReadStringer); ok {
		s, err = r.ReadString('\n')
	} else {
		s, err = readString(stream.reader, '\n')
	}
	if len(s) > 0 {
		return String(chomp(s)), nil
	}
	if err == io.EOF {
		if stream.eofFlag {
			return Null, io.EOF
		}
		return stream.eofValue, nil
	}
	return String(chomp(s)), err
}

func funRead(ctx context.Context, w *World, argv []Node) (Node, error) {
	stream, err := newStreamInput(w, argv)
	if err != nil {
		return nil, err
	}
	value, err := ReadNode(stream.reader)
	if err == io.EOF && !stream.eofFlag {
		return stream.eofValue, nil
	}
	if err != nil {
		var numError *strconv.NumError
		if errors.As(err, &numError) {
			return callHandler[*ParseError](ctx, w, true, &ParseError{
				str:           String(numError.Num),
				ExpectedClass: numberClass,
			})
		}
	}
	return value, err
}

func funReadChar(_ context.Context, w *World, argv []Node) (Node, error) {
	stream, err := newStreamInput(w, argv)
	if err != nil {
		return nil, err
	}
	ch, _, err := stream.reader.ReadRune()
	if err == io.EOF {
		if stream.eofFlag {
			return Null, io.EOF
		}
		return stream.eofValue, nil
	}
	return Rune(ch), err
}

func funClose(ctx context.Context, w *World, arg Node) (Node, error) {
	c, ok := arg.(io.Closer)
	if !ok {
		return callHandler[Node](ctx, w, true, StreamError{Stream: arg})
	}
	return Null, c.Close()
}

func funCreateStringInputStream(ctx context.Context, w *World, arg Node) (Node, error) {
	s, err := ExpectClass[String](ctx, w, arg)
	if err != nil {
		return nil, err
	}
	return _ReaderNode{_Reader: strings.NewReader(s.String())}, nil
}

func funCreateStringOutputStream(ctx context.Context, w *World) (Node, error) {
	return &StringBuilder{}, nil
}

func funGetOutputStreamString(ctx context.Context, w *World, arg Node) (Node, error) {
	stringer, ok := arg.(fmt.Stringer) // expect StringBuilder
	if !ok {
		return nil, ErrNotSupportType
	}
	return String(stringer.String()), nil
}

type inputStream struct {
	_Reader
	io.Closer
}

func openInputFile(ctx context.Context, w *World, fname Node) (*inputStream, error) {
	filename, err := ExpectClass[String](ctx, w, fname)
	if err != nil {
		return nil, err
	}
	reader, err := os.Open(filename.String())
	if err != nil {
		return nil, err
	}
	return &inputStream{_Reader: bufio.NewReader(reader), Closer: reader}, nil
}

func funOpenInputFile(ctx context.Context, w *World, arg Node) (Node, error) {
	return openInputFile(ctx, w, arg)
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
	symbol, err := ExpectClass[Symbol](ctx, w, varName)
	if err != nil {
		return nil, err
	}
	filename, _, err := w.ShiftAndEvalCar(ctx, param)
	if err != nil {
		return nil, err
	}
	stream, err := openInputFile(ctx, w, filename)
	if err != nil {
		return nil, err
	}
	defer stream.Close()

	nw := w.Let(&Pair{Key: symbol, Value: stream})
	return Progn(ctx, nw, list)
}

type _OutputFileStream struct {
	*bufio.Writer
	closer io.Closer
}

func (o *_OutputFileStream) Close() error {
	o.Writer.Flush()
	return o.closer.Close()
}

func openOutputFile(ctx context.Context, w *World, fnameNode Node) (*_OutputFileStream, error) {
	filename, err := ExpectClass[String](ctx, w, fnameNode)
	if err != nil {
		return nil, err
	}
	writer, err := os.Create(filename.String())
	if err != nil {
		return nil, err
	}
	return &_OutputFileStream{Writer: bufio.NewWriter(writer), closer: writer}, nil
}

func funOpenOutputFile(ctx context.Context, w *World, arg Node) (Node, error) {
	return openOutputFile(ctx, w, arg)
}

func cmdWithOpenOutputFile(ctx context.Context, w *World, list Node) (Node, error) {
	param, list, err := Shift(list)
	if err != nil {
		return nil, err
	}
	varName, param, err := Shift(param)
	if err != nil {
		return nil, err
	}
	symbol, err := ExpectClass[Symbol](ctx, w, varName)
	if err != nil {
		return nil, err
	}
	filename, _, err := w.ShiftAndEvalCar(ctx, param)
	if err != nil {
		return nil, err
	}
	stream, err := openOutputFile(ctx, w, filename)
	if err != nil {
		return nil, err
	}
	defer stream.Close()

	nw := w.Let(&Pair{Key: symbol, Value: stream})
	return Progn(ctx, nw, list)
}

func funProbeFile(ctx context.Context, w *World, arg Node) (Node, error) {
	_fname, err := ExpectClass[String](ctx, w, arg)
	if err != nil {
		return nil, err
	}
	fname := _fname.String()
	_, err = os.Stat(fname)
	if err != nil {
		return Null, nil
	}
	return True, nil
}

func funFileLength(ctx context.Context, w *World, first, second Node) (Node, error) {
	_fname, err := ExpectClass[String](ctx, w, first)
	if err != nil {
		return nil, err
	}
	fname := _fname.String()

	_n, err := ExpectClass[Integer](ctx, w, second)
	if err != nil {
		return nil, err
	}
	n := int64(_n)

	stat, err := os.Stat(fname)
	if err != nil {
		return Null, nil
	}
	return Integer(stat.Size() * 8 / n), nil
}

func cmdWithStandardInput(ctx context.Context, w *World, node Node) (Node, error) {
	_stream, node, err := w.ShiftAndEvalCar(ctx, node)
	if err != nil {
		return nil, err
	}
	stream, ok := _stream.(_ReaderNode)
	if !ok {
		return nil, fmt.Errorf("%v: %w", _stream, ErrExpectedStream)
	}
	save := w.stdin
	w.stdin = stream
	result, err := Progn(ctx, w, node)
	w.stdin = save
	return result, err
}
