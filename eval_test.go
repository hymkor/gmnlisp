package gommon

import (
	"fmt"
	"os"
	"testing"
)

func TestEval(t *testing.T) {
	fmt.Println("Start TestEval")
	_,err := ParseString("(print 1 (quote 2 3) 4 5)").Eval()
	if err != nil {
		fmt.Fprint(os.Stderr,err.Error())
	}
	fmt.Println("End TestEval")
}
