package gommon

import (
	"errors"
	"fmt"
	"os"
)

func CmdQuote(this Node) (Node, error) {
	return this, nil
}

func ForEachList(this Node, f func(Node) error) error {
	for {
		cons, ok := this.(*Cons)
		if !ok {
			return fmt.Errorf("Not a list: %s", Node2String(this))
		}
		one, err := cons.Car.Eval()
		if err != nil {
			return err
		}
		if err := f(one); err != nil {
			return err
		}
		if IsNull(cons.Cdr) {
			return nil
		}
		this = cons.Cdr
	}
}

func List2Array(this Node) ([]Node, error) {
	result := []Node{}
	err := ForEachList(this, func(one Node) error {
		result = append(result, one)
		return nil
	})
	return result, err
}

func CmdPrint(this Node) (Node, error) {
	dem := ""
	err := ForEachList(this, func(one Node) error {
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
	err := ForEachList(this, func(one Node) error {
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
