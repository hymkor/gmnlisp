package gommon

import (
	"fmt"
	"os"
)

func CmdPrint(this Atom) (Atom, error) {
	if cons, ok := this.(*Cons); ok {
		for cons != nil {
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
			} else if cons.Cdr == nil {
				break
			} else {
				cons.Cdr.WriteTo(os.Stdout)
				break
			}
		}
	} else {
		this.WriteTo(os.Stdout)
	}
	fmt.Println()
	return nil, nil
}

func CmdQuote(this Atom) (Atom, error) {
	return this, nil
}
