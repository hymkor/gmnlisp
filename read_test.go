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

	code = "(a (b . (c . d)) e)"
	println(code)
	print("-> ")
	ReadString(code).WriteTo(os.Stdout)
	println()

	code = "(car (cons 1 2))"
	println(code)
	print("-> ")
	result, err := ReadString(code).Eval()
	if err != nil {
		println(err.Error())
	} else {
		result.WriteTo(os.Stdout)
	}
	println()

	code = "(cdr (cons 1 2))"
	println(code)
	print("-> ")
	result, err = ReadString(code).Eval()
	if err != nil {
		println(err.Error())
	} else {
		result.WriteTo(os.Stdout)
	}
	println()

}
