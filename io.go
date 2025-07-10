package gmnlisp

import (
	"context"
	"errors"
	"io"
	"os"
	"strconv"
	"strings"
)

var streamClass = registerClass(&BuiltInClass{
	name: NewSymbol("<stream>"),
	instanceP: func(value Node) bool {
		if _, ok := value.(io.Reader); ok {
			return true
		}
		_, ok := value.(io.Writer)
		return ok
	},
	create: func() Node {
		return &StringBuilder{}
	},
	super: []Class{ObjectClass, BuiltInClassObject},
})

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
				ExpectedClass: streamClass,
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

func funPreviewChar(ctx context.Context, w *World, argv []Node) (Node, error) {
	stream, err := newStreamInput(w, argv)
	if err != nil {
		return nil, err
	}
	type runeUnreader interface {
		UnreadRune() error
		Node
	}
	ru, ok := stream.reader.(runeUnreader)
	if !ok {
		domainError := &DomainError{
			ExpectedClass: streamClass,
		}
		if len(argv) > 0 {
			domainError.Object = argv[0]
		} else {
			domainError.Object = w.stdin
		}
		return nil, domainError
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
	if err != nil {
		return nil, err
	}
	return Rune(ch), ru.UnreadRune()
}

func funClose(ctx context.Context, w *World, arg Node) (Node, error) {
	c, ok := arg.(io.Closer)
	if !ok {
		return callHandler[Node](ctx, w, true, &DomainError{
			Object:        arg,
			ExpectedClass: streamClass,
		})
	}
	return Null, c.Close()
}

func funFinishOutput(ctx context.Context, w *World, arg Node) (Node, error) {
	W, ok := arg.(interface{ Flush() error })
	if !ok {
		return callHandler[Node](ctx, w, true, &DomainError{
			Object:        arg,
			ExpectedClass: streamClass,
		})
	}
	W.Flush()
	return Null, nil
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
	return callHandler[Node](ctx, w, true, &DomainError{
		Object:        stream,
		ExpectedClass: streamClass,
	})
}

type filePositioner interface {
	FilePosition() (int64, error)
	Node
}

func funFilePosition(ctx context.Context, w *World, node Node) (Node, error) {
	if f, ok := node.(filePositioner); ok {
		ret, err := f.FilePosition()
		return Integer(ret), err
	}
	return nil, &DomainError{
		Object:        node,
		ExpectedClass: streamClass,
	}
}

type setFilePositioner interface {
	SetFilePosition(int64) (int64, error)
	Node
}

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
		ExpectedClass: streamClass,
	}
}

func openOutputFile(ctx context.Context, w *World, fnameNode Node) (*outputStream, error) {
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
	type readerInterface interface {
		_Reader
		Node
	}
	stream, err := ExpectInterface[readerInterface](ctx, w, _stream, streamClass)
	if err != nil {
		return nil, err
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
	stream, err := ExpectInterface[writer](ctx, w, _stream, streamClass)
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
	stream, err := ExpectInterface[writer](ctx, w, _stream, streamClass)
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

func funStreamReadyP(ctx context.Context, w *World, node Node) (Node, error) {
	type queryable interface {
		QueryStreamReady() (Node, error)
		Node
	}
	x, err := ExpectInterface[queryable](ctx, w, node, streamClass)
	if err != nil {
		return Null, err
	}
	return x.QueryStreamReady()
}
