package common

import (
	"testing"

	. "github.com/hymkor/gmnlisp"
)

func TestDoTimes(t *testing.T) {
	assertEqual(t, `(let (n (sum 0))
		(dotimes (n 4) (setq sum (+ sum n)))
		sum)`, Integer(0+1+2+3))
}

func TestDoList(t *testing.T) {
	assertEqual(t, `(let (n (sum 0))
		(dolist (n '(1 3 5 7))
			(setq sum (+ sum n)))
		sum)`, Integer(1+3+5+7))
}
