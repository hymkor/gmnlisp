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
			return nil, &DomainError{
				Object:        argv[0],
				ExpectedClass: readerNodeClass,
			}
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

func funReadLine(ctx context.Context, w *World, argv []Node) (Node, error) {
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
			return callHandler[Node](ctx, w, true, EndOfStream{
				Stream: argv[0],
			})
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
	if err == io.EOF {
		if !stream.eofFlag {
			return stream.eofValue, nil
		}
		return callHandler[Node](ctx, w, true, EndOfStream{
			Stream: argv[0],
		})
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

func funReadByte(ctx context.Context, w *World, argv []Node) (Node, error) {
	stream, err := newStreamInput(w, argv)
	if err != nil {
		return nil, err
	}
	ch, err := stream.reader.ReadByte()
	if err == io.EOF {
		if stream.eofFlag {
			return callHandler[Node](ctx, w, true, EndOfStream{
				Stream: argv[0],
			})
		}
		return stream.eofValue, nil
	}
	return Integer(ch), err
}

func funReadChar(ctx context.Context, w *World, argv []Node) (Node, error) {
	stream, err := newStreamInput(w, argv)
	if err != nil {
		return nil, err
	}
	ch, _, err := stream.reader.ReadRune()
	if err == io.EOF {
		if stream.eofFlag {
			return callHandler[Node](ctx, w, true, EndOfStream{
				Stream: argv[0],
			})
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
	_Reader  *bufio.Reader
	file     *os.File
	isClosed bool
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
	z, err := i.file.Seek(0, os.SEEK_CUR)
	if err != nil {
		return 0, err
	}
	return z - int64(i._Reader.Buffered()), nil
}

func (i *inputStream) SetFilePosition(n int64) (int64, error) {
	ret, err := i.file.Seek(n, os.SEEK_SET)
	i._Reader.Reset(i.file)
	return ret, err
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
	return &inputStream{_Reader: bufio.NewReader(reader), file: reader}, nil
}

func funOpenInputFile(ctx context.Context, w *World, args []Node) (Node, error) {
	return openInputFile(ctx, w, args[0])
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

func funWriteByte(ctx context.Context, w *World, z, stream Node) (Node, error) {
	data, err := ExpectClass[Integer](ctx, w, z)
	if err != nil {
		return nil, err
	}
	if w, ok := stream.(io.ByteWriter); ok {
		w.WriteByte(byte(int(data)))
		return data, nil
	}
	if w, ok := stream.(io.Writer); ok {
		w.Write([]byte{byte(int(data))})
		return data, nil
	}
	return nil, errors.New("not stream")
}

type filePositioner interface {
	FilePosition() (int64, error)
	Node
}

var filePositionerClass = registerNewAbstractClass[filePositioner]("<stream-file-position>")

func funFilePosition(ctx context.Context, w *World, node Node) (Node, error) {
	if f, ok := node.(filePositioner); ok {
		ret, err := f.FilePosition()
		return Integer(ret), err
	}
	return nil, &DomainError{
		Object:        node,
		ExpectedClass: filePositionerClass,
	}
}

type setFilePositioner interface {
	SetFilePosition(int64) (int64, error)
	Node
}

var setFilePositionerClass = registerNewAbstractClass[setFilePositioner]("<stream-set-file-position>")

func funSetFilePosition(ctx context.Context, w *World, stream, z Node) (Node, error) {
	offset, err := ExpectClass[Integer](ctx, w, z)
	if err != nil {
		return nil, err
	}
	if f, ok := stream.(setFilePositioner); ok {
		ret, err := f.SetFilePosition(int64(offset))
		return Integer(ret), err
	}
	return nil, &DomainError{
		Object:        stream,
		ExpectedClass: setFilePositionerClass,
	}
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
	return newOutputFileStream(writer), nil
}

func funOpenOutputFile(ctx context.Context, w *World, args []Node) (Node, error) {
	return openOutputFile(ctx, w, args[0])
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
	symbol, err := ExpectSymbol(ctx, w, varName)
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

func cmdWithStandardOutput(ctx context.Context, w *World, node Node) (Node, error) {
	_stream, node, err := w.ShiftAndEvalCar(ctx, node)
	if err != nil {
		return nil, err
	}
	type writer interface {
		Node
		io.Writer
	}
	stream, err := ExpectInterface[writer](ctx, w, _stream, outputFileStreamClass)
	if err != nil {
		return nil, err
	}
	save := w.stdout
	w.stdout = stream
	result, err := Progn(ctx, w, node)
	w.stdout = save
	return result, err
}

func cmdWithErrorOutput(ctx context.Context, w *World, node Node) (Node, error) {
	_stream, node, err := w.ShiftAndEvalCar(ctx, node)
	if err != nil {
		return nil, err
	}
	type writer interface {
		Node
		io.Writer
	}
	stream, err := ExpectInterface[writer](ctx, w, _stream, outputFileStreamClass)
	if err != nil {
		return nil, err
	}
	save := w.errout
	w.errout = stream
	result, err := Progn(ctx, w, node)
	w.errout = save
	return result, err
}

func funStreamP(ctx context.Context, w *World, node Node) (Node, error) {
	if _, ok := node.(io.Writer); ok {
		return True, nil
	}
	if _, ok := node.(io.Reader); ok {
		return True, nil
	}
	return Null, nil
}

func funInputStreamP(ctx context.Context, w *World, node Node) (Node, error) {
	if _, ok := node.(io.Reader); ok {
		return True, nil
	}
	return Null, nil
}

func funOutputStreamP(ctx context.Context, w *World, node Node) (Node, error) {
	if _, ok := node.(io.Writer); ok {
		return True, nil
	}
	return Null, nil
}

func funOpenStreamP(ctx context.Context, w *World, node Node) (Node, error) {
	if _, ok := node.(io.Writer); ok {

	} else if _, ok := node.(io.Reader); ok {

	} else {
		return Null, nil
	}
	if t, ok := node.(interface{ IsClosed() bool }); ok && t.IsClosed() {
		return Null, nil
	}
	return True, nil
}
