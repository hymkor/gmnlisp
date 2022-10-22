package gmnlisp

import (
	"testing"
)

func TestMacro(t *testing.T) {
	assertEqual(t, `
		(defmacro dbl (x) (list '* x 2))
		(dbl 3)
	`, Integer(6))

	assertEqual(t, `
		(defmacro dbl  (x) (list '+ x x))
		(defmacro incf (y) (list 'setq y (list '+ y 1)))
		(let ((a1 2))
			(dbl (incf a1)))
	`, Integer(7))

	assertEqual(t, `(list ''foo)`, List(List(NewSymbol("quote"), NewSymbol("foo"))))

	assertEqual(t, `
		(defmacro dolist (pair &rest commands)
		  (let ((key (car pair))
				(values (car (cdr pair))))
			`+"`"+`(mapc (lambda (,key) ,@commands) ,values)
			)
		  )
		(let ((result nil))
		  (dolist (x '(1 2 3)) (setq result (cons x result)))
		  result
		  )`, List(Integer(3), Integer(2), Integer(1)))
}
