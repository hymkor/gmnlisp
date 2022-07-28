package gmnlisp

import (
	"testing"
)

func TestLet(t *testing.T) {
	assertEqual(t, `(let* ((x 2)(y x)) y)`, Integer(2))
	assertEqual(t, `(let ((x 0)) (let ((x 2)(y x)) y))`, Integer(0))
}
