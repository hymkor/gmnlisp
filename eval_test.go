package gommon

import (
	"fmt"
	"os"
	"testing"
)

func TestEval(t *testing.T) {
	fmt.Println("Start TestEval")
	list := ReadString("(print 1 (quote 2 3) 4 5)")
	fmt.Println("Parsing")
	_, err := list.Eval()
	if err != nil {
		fmt.Fprintln(os.Stderr, err.Error())
	}
	fmt.Println("End TestEval")
}
