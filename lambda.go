package gommon

import (
	"errors"
	"fmt"
	"io"
)

type ErrEarlyReturns struct {
	Value Node
	Name  string
}

func (e *ErrEarlyReturns) Error() string {
	if e.Name == "" {
		return "Unexpected (return)"
	}
	return fmt.Sprintf("Unexpected (return-from %s)", e.Name)
}

func CmdReturn(node Node) (Node, error) {
	cons, ok := node.(*Cons)
	if !ok {
		return nil, fmt.Errorf("return: %w", ErrExpectedCons)
	}
	value, err := cons.GetCar().Eval()
	if err != nil {
		return nil, fmt.Errorf("return: %w", err)
	}
	return nil, &ErrEarlyReturns{Value: value, Name: ""}
}

func CmdReturnFrom(node Node) (Node, error) {
	cons, ok := node.(*Cons)
	if !ok {
		return nil, fmt.Errorf("return-from: %w", ErrExpectedCons)
	}
	symbol, ok := cons.Car.(NodeSymbol)
	if !ok {
		return nil, fmt.Errorf("return-from: %w", ErrExpectedSymbol)
	}
	cons, ok = cons.Cdr.(*Cons)
	if !ok {
		return nil, fmt.Errorf("return-from: %w", ErrExpectedCons)
	}
	value, err := cons.GetCar().Eval()
	if err != nil {
		return nil, fmt.Errorf("return-from: %w", err)
	}
	return nil, &ErrEarlyReturns{Value: value, Name: string(symbol)}
}

func progn(c Node) (Node, error) {
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

func CmdProgn(c Node) (Node, error) {
	result, err := progn(c)
	if err != nil {
		return result, fmt.Errorf("progn: %w", err)
	}
	return result, err
}

type NodeLambda struct {
	param []string
	code  Node
	name  string
}

func CmdLambda(node Node) (Node, error) {
	rv, err := NewLambda(node, "")
	if err != nil {
		return rv, fmt.Errorf("lambda: %w", err)
	}
	return rv, nil
}

func NewLambda(node Node, blockName string) (Node, error) {
	// (lambda (param) code)

	cons, ok := node.(*Cons)
	if !ok {
		return nil, fmt.Errorf("%w for parameter list", ErrExpectedCons)
	}
	params := []string{}
	if err := ForEachQuote(cons.Car, func(n Node) error {
		name, ok := n.(NodeSymbol)
		if !ok {
			return ErrExpectedSymbol
		}
		params = append(params, string(name))
		return nil
	}); err != nil {
		return nil, err
	}

	return &NodeLambda{
		param: params,
		code:  cons.GetCdr(),
		name:  blockName,
	}, nil
}

func (nl *NodeLambda) PrintTo(w io.Writer) {
	nl.prinX(w, true)
}

func (nl *NodeLambda) PrincTo(w io.Writer) {
	nl.prinX(w, false)
}

func (NL *NodeLambda) prinX(w io.Writer, rich bool) {
	io.WriteString(w, "(lambda (")
	dem := ""
	for _, name := range NL.param {
		io.WriteString(w, dem)
		io.WriteString(w, name)
		dem = " "
	}
	io.WriteString(w, ") ")
	if cons, ok := NL.code.(*Cons); ok {
		cons.writeToWithoutKakko(w, rich)
	} else {
		if rich {
			NL.code.PrintTo(w)
		} else {
			NL.code.PrincTo(w)
		}
	}
	io.WriteString(w, ")")
}

func (*NodeLambda) Null() bool {
	return false
}

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
			globals[name] = NullValue
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
	var errEarlyReturns *ErrEarlyReturns

	result, err := progn(NL.code)
	if errors.As(err, &errEarlyReturns) && errEarlyReturns.Name == string(NL.name) {
		return errEarlyReturns.Value, nil
	}
	return result, err
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
		return nil, fmt.Errorf("defun: %w", ErrExpectedCons)
	}
	_name, ok := cons.Car.(NodeSymbol)
	if !ok {
		return nil, fmt.Errorf("defun: %w", ErrExpectedSymbol)
	}
	name := string(_name)

	lambda, err := NewLambda(cons.Cdr, string(_name))
	if err != nil {
		return nil, fmt.Errorf("defun: %w", err)
	}
	globals[name] = lambda
	return lambda, nil
}

func CmdBlock(node Node) (Node, error) {
	cons, ok := node.(*Cons)
	if !ok {
		return nil, fmt.Errorf("block: %w", ErrExpectedCons)
	}
	_name, ok := cons.Car.(NodeSymbol)
	if !ok {
		return nil, fmt.Errorf("block: %w", ErrExpectedSymbol)
	}
	name := string(_name)

	var errEarlyReturns *ErrEarlyReturns
	rv, err := progn(cons.Cdr)
	if errors.As(err, &errEarlyReturns) && errEarlyReturns.Name == name {
		return errEarlyReturns.Value, nil
	}
	return rv, err
}
