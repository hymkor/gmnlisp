package gommon

import (
	"os"
	"testing"
)

func TestParse(t *testing.T) {
	for _, code := range []string{
		"(a (b c d) e d)",
		"(a (b () c d) e d)",
		"(a (b . (c . d)) e)"} {

		println(code)
		print("-> ")
		compiled, err := ReadString(code)
		if err != nil {
			t.Fatalf("Error: %s: %s", code, err.Error())
			return
		}
		compiled.WriteTo(os.Stdout)
		println()
	}

	for _, code := range []string{
		"(car (cons 1 2))",
		"(cdr (cons 1 2))"} {

		println(code)
		print("-> ")
		compiled, err := ReadString(code)
		if err != nil {
			t.Fatalf("Error: %s: %s", code, err.Error())
			return
		}
		result, err := compiled.Eval()
		if err != nil {
			t.Fatalf("Error: %s: %s", code, err.Error())
			return
		}
		result.WriteTo(os.Stdout)
		println()
	}
}
