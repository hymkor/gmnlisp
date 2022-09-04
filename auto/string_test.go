package auto

import (
	"testing"

	"github.com/hymkor/gmnlisp"
)

func assertEqual(t *testing.T, equation string, expect gmnlisp.Node) {
	w := gmnlisp.New()
	w = Using(w)
	if e := w.Assert(equation, expect); e != "" {
		t.Fatal(e)
	}
}

func TestString(t *testing.T) {
	assertEqual(t, `(string-append "12345" "67890")`, gmnlisp.String("1234567890"))
	assertEqual(t, `(strcase "abcdef")`, gmnlisp.String("ABCDEF"))
}
