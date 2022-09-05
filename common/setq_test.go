package common

import (
	"testing"

	. "github.com/hymkor/gmnlisp"
)

func assertEqual(t *testing.T, equation string, expect Node) {
	w := New().Let(Functions)
	if e := w.Assert(equation, expect); e != "" {
		t.Fatal(e)
	}
}

func TestDefvar(t *testing.T) {
	assertEqual(t, `(defvar a "ahaha")`, NewSymbol("a"))
	assertEqual(t, `(defvar a "ahaha")(defvar a "ihihi") a`, String("ahaha"))

	assertEqual(t, `
		(defvar counter 0)
		(defvar a (setq counter (1+ counter)))
		(defvar a (setq counter (1+ counter)))
		counter`, Integer(1))
}

func TestDefparameter(t *testing.T) {
	assertEqual(t, `(defparameter a "ahaha")`, NewSymbol("a"))
	assertEqual(t, `(defparameter a "ahaha")(defparameter a "ihihi") a`, String("ihihi"))
}
