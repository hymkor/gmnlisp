package gommon

import (
	"fmt"
	"os"
)

func cmdPrinX(this Node, f func(node Node)) (Node, error) {
	dem := ""
	err := ForEachEval(this, func(one Node) error {
		fmt.Print(dem)
		f(one)
		dem = " "
		return nil
	})
	return NullValue, err
}

func CmdPrint(this Node) (Node, error) {
	fmt.Println()
	return cmdPrinX(this, func(node Node) { node.PrintTo(os.Stdout) })
}

func CmdPrin1(this Node) (Node, error) {
	return cmdPrinX(this, func(node Node) { node.PrintTo(os.Stdout) })
}

func CmdPrinc(this Node) (Node, error) {
	return cmdPrinX(this, func(node Node) { node.PrincTo(os.Stdout) })
}
