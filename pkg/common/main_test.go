package common

import (
	"testing"

	. "github.com/hymkor/gmnlisp"
)

func assertEqual(t *testing.T, equation string, expect Node) {
	w := New()
	if e := w.Assert(equation, expect); e != "" {
		t.Helper()
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

func TestWhen(t *testing.T) {
	assertEqual(t, `(let ((a 0)) (when (> 5 3) (setq a 1)) a)`, Integer(1))
	assertEqual(t, `(let ((a 0)) (when (< 5 3) (setq a 1)) a)`, Integer(0))
}

func TestUnless(t *testing.T) {
	assertEqual(t, `(let ((a 0)) (unless (> 5 3) (setq a 1)) a)`, Integer(0))
	assertEqual(t, `(let ((a 0)) (unless (< 5 3) (setq a 1)) a)`, Integer(1))
}

func TestSetf(t *testing.T) {
	assertEqual(t, `(defglobal x (list 1 2 3 4))
					(setf (nth 2 x) 0)
					x`, List(Integer(1), Integer(2), Integer(0), Integer(4)))
	assertEqual(t, `(defglobal x (list 1 2 3 4))
					(setf (nthcdr 2 x) (list 7))
					x`, List(Integer(1), Integer(2), Integer(7)))

	assertEqual(t, `(defglobal x (list 1 2 3 4))
					(setf (cadr x) 0)
					x`, List(Integer(1), Integer(0), Integer(3), Integer(4)))

	assertEqual(t, `(defglobal x (list 1 2 3 4))
					(setf (caddr x) 0)
					x`, List(Integer(1), Integer(2), Integer(0), Integer(4)))

	assertEqual(t, `(defglobal x (list 1 2 3 4))
					(setf (cadddr x) 0)
					x`, List(Integer(1), Integer(2), Integer(3), Integer(0)))

	assertEqual(t, `(defglobal x (list 1 2 3 4))
					(setf (cddr x) (list 0))
					x`, List(Integer(1), Integer(2), Integer(0)))

	assertEqual(t, `(defglobal x (list 1 2 3 4))
					(setf (cdddr x) (list 0))
					x`, List(Integer(1), Integer(2), Integer(3), Integer(0)))

}
