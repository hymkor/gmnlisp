package gommon

import (
	"fmt"
	"os"
)

func CmdPrint(this *Cons) (*Cons, error) {
	list, err := this.Eval()
	if err != nil {
		return nil, err
	}
	list.WriteTo(os.Stdout)
	fmt.Println()
	return nil, nil
}

func CmdQuote(this *Cons) (*Cons, error) {
	return this, nil
}
