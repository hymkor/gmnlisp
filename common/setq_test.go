package common

import (
	"testing"

	. "github.com/hymkor/gmnlisp"
)

func assertEqual(t *testing.T, equation string, expect Node) {
	w := New()
	w = Using(w)
	if e := w.Assert(equation, expect); e != "" {
		t.Fatal(e)
	}
}

func TestDefvar(t *testing.T) {
	assertEqual(t, `(defvar a "ahaha")`, Symbol("a"))
	assertEqual(t, `(defvar a "ahaha")(defvar a "ihihi") a`, String("ahaha"))

	assertEqual(t, `
		(defvar counter 0)
		(defvar a (setq counter (1+ counter)))
		(defvar a (setq counter (1+ counter)))
		counter`, Integer(1))
}
