package gommon

import (
	"errors"
	"fmt"
	"io"
)

func CmdProgn(c Node) (Node, error) {
	var last Node
	for !IsNull(c) {
		cons, ok := c.(*Cons)
		if !ok {
			return nil, fmt.Errorf("progn: %w", ErrExpectedCons)
		}
		var err error
		last, err = cons.GetCar().Eval()
		if err != nil {
			return nil, err
		}
		c = cons.Cdr
	}
	return last, nil
}

type NodeLambda struct {
	param []string
	code  Node
}

func CmdLambda(node Node) (Node, error) {
	// (lambda (param) code)

	cons, ok := node.(*Cons)
	if !ok {
		return nil, fmt.Errorf("lambda: %w for parameter list", ErrExpectedCons)
	}
	params := []string{}
	if err := ForEachQuote(cons.Car, func(n Node) error {
		name, ok := n.(NodeSymbol)
		if !ok {
			return fmt.Errorf("lambda: %w", ErrExpectedSymbol)
		}
		params = append(params, string(name))
		return nil
	}); err != nil {
		return nil, err
	}

	return &NodeLambda{
		param: params,
		code:  cons.GetCdr(),
	}, nil
}

func (NL *NodeLambda) WriteTo(w io.Writer) (int64, error) {
	var n int64
	if err := write(&n, w, "(lambda ("); err != nil {
		return n, err
	}
	dem := ""
	for _, name := range NL.param {
		if err := write(&n, w, dem); err != nil {
			return n, err
		}
		if err := write(&n, w, name); err != nil {
			return n, err
		}
		dem = " "
	}
	write(&n, w, ") ")
	var _n int64
	var err error
	if cons, ok := NL.code.(*Cons); ok {
		_n, err = cons.writeToWithoutKakko(w)
	} else {
		_n, err = NL.code.WriteTo(w)
	}
	n += _n
	if err != nil {
		return n, err
	}
	err = write(&n, w, ")")
	return n, err
}

func (*NodeLambda) Null() bool {
	return false
}

var globals = map[string]Node{}

func (NL *NodeLambda) Call(n Node) (Node, error) {
	backups := map[string]Node{}
	nobackups := map[string]struct{}{}
	for _, name := range NL.param {
		if value, ok := globals[name]; ok {
			backups[name] = value
		} else {
			nobackups[name] = struct{}{}
		}

		if cons, ok := n.(*Cons); ok {
			var err error
			globals[name], err = cons.GetCar().Eval()
			if err != nil {
				return nil, err
			}
			n = cons.GetCdr()
		} else {
			globals[name] = &Null{}
		}
	}
	defer func() {
		for name := range nobackups {
			delete(globals, name)
		}
		for name, value := range backups {
			globals[name] = value
		}
	}()
	return CmdProgn(NL.code)
}

func (NL *NodeLambda) Eval() (Node, error) {
	return NL, nil
}

func (*NodeLambda) Equals(Node) bool {
	return false
}

func CmdDefun(node Node) (Node, error) {
	cons, ok := node.(*Cons)
	if !ok {
		return nil, errors.New("Not a list")
	}
	_name, ok := cons.Car.(NodeSymbol)
	if !ok {
		return nil, errors.New("Not a Symbol")
	}
	name := string(_name)

	lambda, err := CmdLambda(cons.Cdr)
	if err != nil {
		return nil, err
	}
	globals[name] = lambda
	return lambda, nil
}
