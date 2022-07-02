package gommon

import (
	"errors"
	"io"
)

func CmdProgn(c Node) (Node, error) {
	var last Node
	for !IsNull(c) {
		cons, ok := c.(*Cons)
		if !ok {
			return nil, errors.New("Not a List")
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
		return nil, errors.New("lambda: parameter is not cons")
	}
	params := []string{}
	if err := ForEachQuote(cons.Car, func(n Node) error {
		name, ok := n.(NodeSymbol)
		if !ok {
			return errors.New("lambda: parameter list is not symbol")
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
	}
	write(&n, w, ") ")
	_n, err := NL.code.WriteTo(w)
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
			globals[name] = cons.GetCar()
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
