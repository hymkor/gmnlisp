package gommon

import (
	"fmt"
	"os"
)

func CmdPrint(this Node) (Node, error) {
	if cons, ok := this.(*Cons); ok {
		delim := ""
		for cons != nil && !cons.Null() {
			fmt.Print(delim)
			delim = " "
			if cons1, ok := cons.Car.(*Cons); ok {
				val, err := cons1.Eval()
				if err != nil {
					return nil, err
				}
				val.WriteTo(os.Stdout)
			} else if cons.Car != nil {
				cons.Car.WriteTo(os.Stdout)
			} else {
				fmt.Fprint(os.Stdout, "<nil>")
			}
			if cons1, ok := cons.Cdr.(*Cons); ok {
				cons = cons1
			} else if cons.Cdr == nil || cons.Cdr.Null() {
				break
			} else {
				cons.Cdr.WriteTo(os.Stdout)
				break
			}
		}
	} else if this != nil && !this.Null() {
		this.WriteTo(os.Stdout)
	}
	fmt.Println()
	return nil, nil
}

func CmdQuote(this Node) (Node, error) {
	return this, nil
}

func ForEachList(this Node, f func(Node) error) error {
	for {
		cons, ok := this.(*Cons)
		if !ok {
			return fmt.Errorf("EvalParameters: Not List")
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

func CmdPlus(this Node) (Node, error) {
	var result NodeInteger
	err := ForEachList(this, func(one Node) error {
		value, ok := one.(NodeInteger)
		if !ok {
			return fmt.Errorf("Not A Number(1)")
		}
		result += value
		return nil
	})
	return result, err
}
