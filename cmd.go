package gommon

import (
	"fmt"
	"os"
)

var Symbol = map[string]interface{}{}

func CmdSetq(this *Node) (*Node, error) {
	name, ok := this.Car.(string)
	if !ok {
		return nil, fmt.Errorf("setq: invalid variable-name")
	}
	var err error
	Symbol[name], err = this.Cdr.Eval()
	return nil, err
}

func CmdPrint(this *Node) (*Node, error) {
	list, err := this.Eval()
	if err != nil {
		return nil, err
	}
	list.Print(os.Stdout)
	fmt.Println()
	return nil, nil
}

func CmdQuote(this *Node) (*Node, error) {
	return this, nil
}
