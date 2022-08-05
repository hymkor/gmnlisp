package gmnlisp

import (
	"testing"
)

func TestLet(t *testing.T) {
	assertEqual(t, `(let* ((x 2)(y x)) y)`, Integer(2))
	assertEqual(t, `(let ((x 0)) (let ((x 2)(y x)) y))`, Integer(0))
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

func TestDefparameter(t *testing.T) {
	assertEqual(t, `(defparameter a "ahaha")`, Symbol("a"))
	assertEqual(t, `(defparameter a "ahaha")(defparameter a "ihihi") a`, String("ihihi"))
}

func TestSetf(t *testing.T) {
	assertEqual(t, `(defvar x)
					(setf (car (setq x (cons 1 2))) 3)
					x`, &Cons{Integer(3), Integer(2)})
	assertEqual(t, `(defparameter x (cons 1 2))
					(setf (cdr x) 3)
					x`, &Cons{Integer(1), Integer(3)})
}
