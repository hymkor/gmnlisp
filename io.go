package gmnlisp

import (
	"bufio"
	"context"
	"fmt"
	"io"
	"os"
	"strings"
)

type _Dummy struct{}

func (d _Dummy) Eval(context.Context, *World) (Node, error) {
	return d, nil
}

func (d _Dummy) Equals(Node, EqlMode) bool {
	return false
}

func (d _Dummy) PrintTo(w io.Writer, m PrintMode) (int, error) {
	return io.WriteString(w, "(binary)")
}

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
		this.eofFlag = HasValue(argv[1])
		fallthrough
	case 1:
		var ok bool
		this.reader, ok = argv[0].(_Reader)
		if !ok {
			return nil, fmt.Errorf("Expected Reader `%s`", ToString(argv[0], PRINT))
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

var defReadLine = &Function{Max: 3, F: funReadLine}

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

var defRead = &Function{Max: 3, F: funRead}

func funRead(_ context.Context, w *World, argv []Node) (Node, error) {
	stream, err := newStreamInput(w, argv)
	if err != nil {
		return nil, err
	}
	value, err := ReadNode(stream.reader)
	if err == io.EOF && !stream.eofFlag {
		return stream.eofValue, nil
	}
	return value, err
}

func funClose(_ context.Context, _ *World, argv []Node) (Node, error) {
	c, ok := argv[0].(io.Closer)
	if !ok {
		return nil, fmt.Errorf("Expected Closer `%s`", ToString(argv[0], PRINT))
	}
	return Null, c.Close()
}

func funCreateStringInputStream(ctx context.Context, w *World, list []Node) (Node, error) {
	s, ok := list[0].(StringTypes)
	if !ok {
		return nil, ErrExpectedString
	}
	return _ReaderNode{_Reader: strings.NewReader(s.String())}, nil
}

func cmdCreateStringOutputStream(ctx context.Context, w *World, list Node) (Node, error) {
	type StringBuilder struct {
		_Dummy
		*strings.Builder
	}
	return &StringBuilder{Builder: &strings.Builder{}}, nil
}

func funGetOutputStreamString(ctx context.Context, w *World, list []Node) (Node, error) {
	stringer, ok := list[0].(fmt.Stringer) // expect StringBuilder
	if !ok {
		return nil, ErrNotSupportType
	}
	return String(stringer.String()), nil
}

type inputStream struct {
	_Dummy
	_Reader
	io.Closer
}

func openInputFile(fname Node) (*inputStream, error) {
	filename, ok := fname.(StringTypes)
	if !ok {
		return nil, ErrExpectedString
	}
	reader, err := os.Open(filename.String())
	if err != nil {
		return nil, err
	}
	return &inputStream{_Reader: bufio.NewReader(reader), Closer: reader}, nil
}

func funOpenInputFile(ctx context.Context, w *World, list []Node) (Node, error) {
	return openInputFile(list[0])
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
	symbol, ok := varName.(Symbol)
	if !ok {
		return nil, ErrExpectedSymbol
	}
	filename, _, err := w.ShiftAndEvalCar(ctx, param)
	if err != nil {
		return nil, err
	}
	stream, err := openInputFile(filename)
	if err != nil {
		return nil, err
	}
	defer stream.Close()

	nw := w.Let(&Pair{Key: symbol, Value: stream})
	return Progn(ctx, nw, list)
}

type _OutputFileStream struct {
	_Dummy
	*bufio.Writer
	closer io.Closer
}

func (o *_OutputFileStream) Close() error {
	o.Writer.Flush()
	return o.closer.Close()
}

func openOutputFile(fnameNode Node) (*_OutputFileStream, error) {
	filename, ok := fnameNode.(StringTypes)
	if !ok {
		return nil, ErrExpectedString
	}
	writer, err := os.Create(filename.String())
	if err != nil {
		return nil, err
	}
	return &_OutputFileStream{Writer: bufio.NewWriter(writer), closer: writer}, nil
}

func funOpenOutputFile(ctx context.Context, w *World, list []Node) (Node, error) {
	return openOutputFile(list[0])
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
	symbol, ok := varName.(Symbol)
	if !ok {
		return nil, ErrExpectedSymbol
	}
	filename, _, err := w.ShiftAndEvalCar(ctx, param)
	if err != nil {
		return nil, err
	}
	stream, err := openOutputFile(filename)
	if err != nil {
		return nil, err
	}
	defer stream.Close()

	nw := w.Let(&Pair{Key: symbol, Value: stream})
	return Progn(ctx, nw, list)
}

func funProbeFile(ctx context.Context, w *World, list []Node) (Node, error) {
	_fname, ok := list[0].(StringTypes)
	if !ok {
		return nil, ErrExpectedString
	}
	fname := _fname.String()
	_, err := os.Stat(fname)
	if err != nil {
		return Null, nil
	}
	return True, nil
}
