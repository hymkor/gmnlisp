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
}

func TestDefparameter(t *testing.T) {
	assertEqual(t, `(defparameter a "ahaha")`, Symbol("a"))
	assertEqual(t, `(defparameter a "ahaha")(defparameter a "ihihi") a`, String("ihihi"))
}
