package gommon

import (
	"os"
	"testing"
)

func TestParse(t *testing.T) {
	Print(ParseString("(a (b c d) e d)"), os.Stdout)
	println()

	Print(ParseString("(a (b () c d) e d)"), os.Stdout)
	println()
}
