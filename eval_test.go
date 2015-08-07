package gommon

import (
	"fmt"
	"testing"
)

func TestEval(t *testing.T) {
	fmt.Println("Start TestEval")
	Eval(ParseString("(print 1 2 3 4 5)"))
	fmt.Println("End TestEval")
}
