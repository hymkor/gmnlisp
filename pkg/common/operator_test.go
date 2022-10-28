package common

import (
	"testing"

	. "github.com/hymkor/gmnlisp"
)

func TestIncf(t *testing.T) {
	assertEqual(t, `(let ((x 1)) (incf x) x)`, Integer(2))
	assertEqual(t, `(let ((x 1.0)) (incf x) x)`, Float(2.0))
	assertEqual(t, `(let ((x 1)) (incf x 2) x)`, Integer(3))
	assertEqual(t, `(let ((x 1.0)) (incf x 2) x)`, Float(3.0))
}

func TestDecf(t *testing.T) {
	assertEqual(t, `(let ((x 1)) (decf x) x)`, Integer(0))
	assertEqual(t, `(let ((x 1.0)) (decf x) x)`, Float(0.0))
	assertEqual(t, `(let ((x 1)) (decf x 2) x)`, Integer(-1))
	assertEqual(t, `(let ((x 1.0)) (decf x 2) x)`, Float(-1))
}
