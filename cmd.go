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

func List2Array(this Node) ([]Node, error) {
	result := []Node{}
	err := ForEachEval(this, func(one Node) error {
		result = append(result, one)
		return nil
	})
	return result, err
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

func CmdCons(this Node) (Node, error) {
	nodes, err := List2Array(this)
	if err != nil {
		return nil, err
	}
	if len(nodes) != 2 {
		return nil, errors.New("cons: parameter number not 2")
	}
	return &Cons{Car: nodes[0], Cdr: nodes[1]}, nil
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
		return nil, fmt.Errorf("Not a list: %s", Node2String(cons))
	}
	return cons.Car, nil
}

func CmdCdr(param Node) (Node, error) {
	var result Node
	err := ForEachEval(param, func(firstArg Node) error {
		cons, ok := firstArg.(*Cons)
		if !ok {
			return fmt.Errorf("Not a list: %s", Node2String(cons))
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
