package gommon

import (
	"os"
	"testing"
)

func TestParse(t *testing.T) {
	ReadString("(a (b c d) e d)").WriteTo(os.Stdout)
	println()

	ReadString("(a (b () c d) e d)").WriteTo(os.Stdout)
	println()
}
