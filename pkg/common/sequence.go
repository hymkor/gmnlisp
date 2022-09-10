package common

import (
	"context"
	"io"

	. "github.com/hymkor/gmnlisp"
)

func getTestParameter(kwargs map[Keyword]Node) (func(context.Context, *World, Node, Node) (bool, error), error) {
	if test, ok := kwargs[":test"]; ok {
		caller, ok := test.(Callable)
		if !ok {
			return nil, ErrExpectedFunction
		}
		return func(c context.Context, w *World, left, right Node) (bool, error) {
			result, err := caller.Call(c, w, List(left, right))
			return HasValue(result), err
		}, nil
	} else {
		return func(_ context.Context, _ *World, left, right Node) (bool, error) {
			return left.Equals(right, STRICT), nil
		}, nil
	}
}

func funFind(c context.Context, w *World, argv []Node, kwargs map[Keyword]Node) (Node, error) {
	expr := argv[0]
	list := argv[1]
	test, err := getTestParameter(kwargs)
	if err != nil {
		return nil, err
	}
	var found Node

	err = SeqEach(list, func(value Node) error {
		eq, err := test(c, w, expr, value)
		if err != nil {
			return err
		}
		if eq {
			found = value
			return io.EOF
		}
		return nil
	})
	if err == io.EOF {
		return found, nil
	}
	return Null, nil
}

func funMember(c context.Context, w *World, argv []Node, kwargs map[Keyword]Node) (Node, error) {
	expr := argv[0]
	list := argv[1]
	test, err := getTestParameter(kwargs)
	if err != nil {
		return nil, err
	}

	for HasValue(list) {
		seq, ok := list.(Sequence)
		if !ok {
			return nil, ErrExpectedSequence
		}
		value, rest, ok, _ := seq.FirstAndRest()
		if !ok {
			break
		}
		eq, err := test(c, w, expr, value)
		if err != nil {
			return nil, err
		}
		if eq {
			return list, nil
		}
		list = rest
	}
	return Null, nil
}

func funPosition(c context.Context, w *World, argv []Node, kwargs map[Keyword]Node) (Node, error) {
	expr := argv[0]
	list := argv[1]
	test, err := getTestParameter(kwargs)
	if err != nil {
		return nil, err
	}
	count := 0

	err = SeqEach(list, func(value Node) error {
		eq, err := test(c, w, expr, value)
		if err != nil {
			return err
		}
		if eq {
			return io.EOF
		}
		count++
		return nil
	})
	if err == io.EOF {
		return Integer(count), nil
	}
	return Null, nil
}

var defConcatenate = &Function{Min: 1, F: funConcatenate}

func funConcatenate(ctx context.Context, w *World, list []Node) (Node, error) {
	if len(list) < 1 {
		return Null, nil
	}
	buffer, err := NewSeqBuilder(list[0])
	if err != nil {
		return nil, err
	}
	for _, element := range list[1:] {
		err := SeqEach(element, func(value Node) error {
			buffer.Add(value)
			return nil
		})
		if err != nil {
			return nil, err
		}
	}
	return buffer.Sequence(), nil
}

func funCoerce(_ context.Context, _ *World, argv []Node) (Node, error) {
	buffer, err := NewSeqBuilder(argv[1])
	if err != nil {
		return nil, ErrNotSupportType
	}
	err = SeqEach(argv[0], func(value Node) error {
		buffer.Add(value)
		return nil
	})
	return buffer.Sequence(), err
}
