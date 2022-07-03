package gommon

import (
	"errors"
	"fmt"
	"io"
	"os"
)

func ForEachQuote(this Node, f func(Node) error) error {
	for {
		cons, ok := this.(*Cons)
		if !ok {
			return fmt.Errorf("Not a list: %s", Node2String(this))
		}
		if err := f(cons.Car); err != nil {
			return err
		}
		if IsNull(cons.Cdr) {
			return nil
		}
		this = cons.Cdr
	}
}

func ForEachEval(this Node, f func(Node) error) error {
	return ForEachQuote(this, func(quotedOne Node) error {
		evalOne, err := quotedOne.Eval()
		if err != nil {
			return err
		}
		return f(evalOne)
	})
}

func CmdPrint(this Node) (Node, error) {
	dem := ""
	err := ForEachEval(this, func(one Node) error {
		fmt.Print(dem)
		_, err1 := one.WriteTo(os.Stdout)
		dem = " "
		return err1
	})
	fmt.Println()
	return &Null{}, err
}

func CmdPlus(this Node) (Node, error) {
	var result NodeInteger
	err := ForEachEval(this, func(one Node) error {
		value, ok := one.(NodeInteger)
		if !ok {
			return fmt.Errorf("Not A Number: %s", Node2String(one))
		}
		result += value
		return nil
	})
	return result, err
}

func CmdCons(node Node) (Node, error) {
	var result [2]Node
	i := 0
	err := ForEachEval(node, func(n Node) error {
		if i >= len(result) {
			return errors.New("cons: parameter number not 2")
		}
		result[i] = n
		i++
		return nil
	})
	return &Cons{Car: result[0], Cdr: result[1]}, err
}

func CmdCar(this Node) (Node, error) {
	cons, ok := this.(*Cons)
	if !ok {
		return nil, fmt.Errorf("Not a list: %s", Node2String(this))
	}

	firstArg, err := cons.Car.Eval()
	if err != nil {
		return nil, err
	}
	cons, ok = firstArg.(*Cons)
	if !ok {
		return nil, fmt.Errorf("Not a list: %s", Node2String(firstArg))
	}
	return cons.Car, nil
}

func CmdCdr(param Node) (Node, error) {
	var result Node
	err := ForEachEval(param, func(firstArg Node) error {
		cons, ok := firstArg.(*Cons)
		if !ok {
			return fmt.Errorf("Not a list: %s", Node2String(firstArg))
		}
		result = cons.Cdr
		return io.EOF
	})
	switch err {
	case io.EOF:
		return result, nil
	case nil:
		return nil, errors.New("Too few parameters")
	default:
		return nil, err
	}
}

func CmdQuote(param Node) (Node, error) {
	cons, ok := param.(*Cons)
	if !ok {
		return nil, fmt.Errorf("Not a list: %s", Node2String(param))
	}
	return cons.Car, nil
}

var T = NodeSymbol("T")

func CmdAtom(param Node) (Node, error) {
	cons, ok := param.(*Cons)
	if !ok {
		return nil, fmt.Errorf("Not a list: %s", Node2String(param))
	}
	if _, ok := cons.Car.(*Cons); ok {
		return &Null{}, nil
	}
	return T, nil
}

func CmdEq(param Node) (Node, error) {
	var first Node
	i := 0
	err := ForEachEval(param, func(node Node) error {
		i++
		if i == 1 {
			first = node
			return nil
		}
		if !first.Equals(node) {
			return io.EOF
		}
		return nil
	})
	if err == io.EOF {
		return &Null{}, nil
	}
	if err == nil {
		return T, nil
	}
	return &Null{}, err
}
