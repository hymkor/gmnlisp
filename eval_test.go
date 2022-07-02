package gommon

import (
	"fmt"
	"os"
	"testing"
)

func TestEval(t *testing.T) {
	fmt.Println("Start TestEval")
	code := "(print 1 (quote 2 3) (+ 4 5))"
	list := ReadString(code)
	fmt.Println("Parsing:", code)
	_, err := list.Eval()
	if err != nil {
		fmt.Fprintln(os.Stderr, err.Error())
	}
	fmt.Println("End TestEval")
}
