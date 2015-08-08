package gommon

import (
	"fmt"
	"testing"
)

func TestEval(t *testing.T) {
	fmt.Println("Start TestEval")
	ParseString("(print 1 2 3 4 5)").Eval()
	fmt.Println("End TestEval")
}
