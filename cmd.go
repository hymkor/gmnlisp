package gommon

import (
	"errors"
	"fmt"
	"io"
	"os"
)

var (
	ErrTooFewOrTooManyArguments = errors.New("Too few or too many arguments")
	ErrExpectedCons             = errors.New("Expected CONS")
	ErrExpectedNumber           = errors.New("Expected a number")
	ErrExpectedSymbol           = errors.New("Expected symbol")
)

func ForEachQuote(this Node, f func(Node) error) error {
	for {
		cons, ok := this.(*Cons)
		if !ok {
			return fmt.Errorf("%w (%s)", ErrExpectedCons, Node2String(this))
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
		one.PrintTo(os.Stdout)
		dem = " "
		return nil
	})
	fmt.Println()
	return NullValue, err
}

func CmdCons(node Node) (Node, error) {
	var result [2]Node
	i := 0
	err := ForEachEval(node, func(n Node) error {
		if i >= len(result) {
			return fmt.Errorf("cons: %w", ErrTooFewOrTooManyArguments)
		}
		result[i] = n
		i++
		return nil
	})
	return &Cons{Car: result[0], Cdr: result[1]}, err
}

func ShiftAndEval(node Node) (Node, Node, error) {
	cons, ok := node.(*Cons)
	if !ok {
		return nil, nil, ErrTooFewOrTooManyArguments
	}
	value, err := cons.GetCar().Eval()
	if err != nil {
		return nil, nil, err
	}
	return value, cons.Cdr, nil
}

func CmdCar(param Node) (Node, error) {
	first, _, err := ShiftAndEval(param)
	if err != nil {
		return nil, fmt.Errorf("car: %w", err)
	}
	cons, ok := first.(*Cons)
	if !ok {
		return nil, fmt.Errorf("car: %w", ErrExpectedCons)
	}
	return cons.Car, nil
}

func CmdCdr(param Node) (Node, error) {
	first, _, err := ShiftAndEval(param)
	if err != nil {
		return nil, fmt.Errorf("cdr: %w", err)
	}
	cons, ok := first.(*Cons)
	if !ok {
		return nil, fmt.Errorf("cdr: %w", ErrExpectedCons)
	}
	return cons.Cdr, nil
}

func CmdQuote(param Node) (Node, error) {
	cons, ok := param.(*Cons)
	if !ok {
		return nil, fmt.Errorf("quote: %w", ErrTooFewOrTooManyArguments)
	}
	return cons.Car, nil
}

func CmdAtom(param Node) (Node, error) {
	cons, ok := param.(*Cons)
	if !ok {
		return nil, fmt.Errorf("atom: %w", ErrExpectedCons)
	}
	if _, ok := cons.Car.(*Cons); ok {
		return NullValue, nil
	}
	return TrueValue, nil
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
		return NullValue, nil
	}
	if err == nil {
		return TrueValue, nil
	}
	return NullValue, fmt.Errorf("eq: %w", err)
}
