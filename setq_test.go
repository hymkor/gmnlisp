package gmnlisp

import (
	"testing"
)

func TestLet(t *testing.T) {
	assertEqual(t, `(let* ((x 2)(y x)) y)`, Integer(2))
	assertEqual(t, `(let ((x 0)) (let ((x 2)(y x)) y))`, Integer(0))
}

func TestGlobal(t *testing.T) {
	assertEqual(t, `(defglobal a "ahaha")`, NewSymbol("a"))
	assertEqual(t, `(defglobal a "ahaha")(defglobal a "ihihi") a`, String("ihihi"))
}

func TestSetf(t *testing.T) {
	assertEqual(t, `(let (x)
					(setf (car (setq x (cons 1 2))) 3)
					x)`, &Cons{Integer(3), Integer(2)})
	assertEqual(t, `(defglobal x (cons 1 2))
					(setf (cdr x) 3)
					x`, &Cons{Integer(1), Integer(3)})
	assertEqual(t, `
		(let ((m (list (cons 1 "A") (cons 2 "B") (cons 3 "C"))))
			(setf (cdr (assoc 1 m)) "X")
		 m)`, List(
		&Cons{Integer(1), String("X")},
		&Cons{Integer(2), String("B")},
		&Cons{Integer(3), String("C")}))

	(assertEqual(t, `
		(let ((m '((1 . "A") (2 . "B") (3 . "C"))) pair )
		  (if (setq pair (assoc 1 m))
			  (setf (cdr pair) "X")
		  )
		  m
		)`, List(
		&Cons{Integer(1), String("X")},
		&Cons{Integer(2), String("B")},
		&Cons{Integer(3), String("C")})))
}

func TestDynamic(t *testing.T) {
	assertEqual(t, `
		(defdynamic *color* 'red)
		(defun what-color () (dynamic *color*))
		(what-color)`, NewSymbol("red"))

	assertEqual(t, `
		(defdynamic hoge 1)
		(setf (dynamic hoge) 3)
		(dynamic hoge)`, Integer(3))
}

func TestDynamicLet(t *testing.T) {
	assertEqual(t, `
		(defdynamic *color* 'red)
		(defun what-color () (dynamic *color*))
		(dynamic-let ((*color* 'green)) (what-color))`, NewSymbol("green"))
}
