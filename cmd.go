package gommon

import (
	"fmt"
	"os"
)

func CmdPrint(this *Node) (*Node, error) {
	list, err := this.Eval()
	if err != nil {
		return nil, err
	}
	list.Dump(os.Stdout)
	fmt.Println()
	return nil, nil
}

func CmdQuote(this *Node) (*Node, error) {
	return this, nil
}
