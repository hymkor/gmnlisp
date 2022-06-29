package gommon

import (
	"os"
	"testing"
)

func TestParse(t *testing.T) {
	code := "(a (b c d) e d)"
	println(code)
	print("-> ")
	ReadString(code).WriteTo(os.Stdout)
	println()

	code = "(a (b () c d) e d)"
	println(code)
	print("-> ")
	ReadString(code).WriteTo(os.Stdout)
	println()
}
