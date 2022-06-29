package gommon

import (
	"fmt"
	"os"
)

func CmdPrint(this Atom) (Atom, error) {
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

func CmdQuote(this Atom) (Atom, error) {
	return this, nil
}
