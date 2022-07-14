package gmnlisp

import (
	"errors"
	"testing"
)

func evalTest(t *testing.T, equation string, expect Node) {
	w := New()
	result, err := w.Interpret(equation)
	if err != nil {
		t.Fatalf("%s: %s", equation, err.Error())
		return
	}
	if !result.Equals(expect) {
		t.Fatalf("%s != %s (was %s)", equation, toString(expect), toString(result))
		return
	}
}

func TestOperators(t *testing.T) {
	evalTest(t, "(+ 1 2)", Integer(3))
	evalTest(t, "(+ 1 2 3)", Integer(6))
	evalTest(t, "(- 10 9)", Integer(1))
	evalTest(t, "(- 10 1 2)", Integer(7))
	evalTest(t, "(* 1 2)", Integer(2))
	evalTest(t, "(* 1 2 3)", Integer(6))
	evalTest(t, "(/ 6 2)", Integer(3))
	evalTest(t, `(+ "1" "2")`, String("12"))
	evalTest(t, `(> 2 1.0)`, True)
	evalTest(t, `(> 2.0 3)`, Null)
	evalTest(t, `(< 2.0 3)`, True)
	evalTest(t, `(< 2 1.0)`, Null)
	evalTest(t, `(<= 2.0 3)`, True)
	evalTest(t, `(<= 3 3)`, True)
	evalTest(t, `(<= 4 3)`, Null)
	evalTest(t, `(>= 2.0 3)`, Null)
	evalTest(t, `(>= 3 3)`, True)
	evalTest(t, `(>= 4 3)`, True)
	evalTest(t, `(> "a" "b")`, Null)
	evalTest(t, `(< "a" "b" "c")`, True)
	evalTest(t, `(< 1 2 3)`, True)
	evalTest(t, `(< 1 2 1)`, Null)
	evalTest(t, `(>= 3 2 2)`, True)
	evalTest(t, `(>= 2 2 2)`, True)
	evalTest(t, `(> 3 2 1)`, True)
	evalTest(t, `(= 1 1)`, True)
	evalTest(t, `(= 1.0 1)`, True)
	evalTest(t, `(= 1 1.0)`, True)
	evalTest(t, `(= 1.0 1.0)`, True)
	evalTest(t, `(= 1.0 1.0 1.0)`, True)
	evalTest(t, `(= 1 2)`, Null)
	evalTest(t, `(= 1 2.0)`, Null)
	evalTest(t, `(= 1.0 2)`, Null)
	evalTest(t, `(= 1 2.0)`, Null)
	evalTest(t, `(= "ABC" "abc")`, True)
	evalTest(t, `(= "ABC" "abcd")`, Null)
	evalTest(t, `(equalp "DEF" "defg")`, Null)
	evalTest(t, `(equalp "GHQ" "ghq")`, True)
	evalTest(t, `(equalp (cons 1 (cons 2 nil)) '(1 2))`, True)
	evalTest(t, `(equalp (cons 1 2) '(1))`, Null)
	evalTest(t, `(equal (list 1 (+ 1 1) (+ 1 2)) '(1 2 3))`, True)
	evalTest(t, `(append '(1 2) '(3 4))`, List(Integer(1), Integer(2), Integer(3), Integer(4)))
}

func TestCmdCond(t *testing.T) {
	evalTest(t, `(cond (nil 1) (T 2))`, Integer(2))
	evalTest(t, `(cond ((equal 1 1) "a") ((equal 1 2) "b"))`, String("a"))
}

func TestCmdCons(t *testing.T) {
	evalTest(t, "(cons 1 2)", &Cons{Car: Integer(1), Cdr: Integer(2)})
	evalTest(t, "(quote (1 . 2))", &Cons{Car: Integer(1), Cdr: Integer(2)})
}

func TestProgn(t *testing.T) {
	evalTest(t, `(progn 1)`, Integer(1))
	evalTest(t, `(progn 1 2)`, Integer(2))
}

func TestEval(t *testing.T) {
	evalTest(t, `
		(progn
			(defun f (a b)
				(+ a b))
			(f 1 2))`, Integer(3))
	evalTest(t, `
		(progn
			(defun f1 (a b)
				(+ a b))
			(f1 1.0 2.0))`, Float(3.0))
	evalTest(t, `
		(let (
				(f2 (lambda (a b) (+ a b)))
			)
			(f2 4 5))`, Integer(9))

	evalTest(t, `
		(progn
			(setq a 0)
			(defun dummy (a b) (+ a b))
			(dummy 7 8)
			a)`, Integer(0))

	evalTest(t, `
		(let ((x 1))
		  (defun f ()
			(list x)))
		(let ((x 2))
			(f))`,
		&Cons{Car: Integer(1), Cdr: Null})
}

func TestLambdaParameter(t *testing.T) {
	w := New()
	_, err := w.Interpret(`(defun f (x y) (+ x y))`)
	if err != nil {
		t.Fatal(err.Error())
	}
	_, err = w.Interpret(`(f 1)`)
	if !errors.Is(err, ErrTooFewArguments) {
		t.Fatal("Few argumenets error did not occur")
	}

	_, err = w.Interpret(`(f 1 2)`)
	if err != nil {
		t.Fatal(err.Error())
	}

	_, err = w.Interpret(`(f 1 2 3)`)
	if !errors.Is(err, ErrTooManyArguments) {
		t.Fatal("Few argumenets error did not occur")
	}
}

func TestWorld(t *testing.T) {
	w1 := New()
	if _, err := w1.Interpret(`(setq a "A")`); err != nil {
		t.Fatal(err.Error())
		return
	}
	value, err := w1.Interpret("a")
	if err != nil {
		t.Fatal(err.Error())
		return
	}
	s, ok := value.(String)
	if !ok {
		t.Fatal("type mismatch")
		return
	}
	if string(s) != "A" {
		t.Fatalf("`%s` != `A`", string(s))
		return
	}

	w2 := New()
	value, err = w2.Interpret(`a`)

	if !errors.Is(err, ErrVariableUnbound) {
		if err == nil {
			t.Fatal("error had to occur")
		} else {
			t.Fatal(err.Error())
		}
		return
	}
}

func TestList(t *testing.T) {
	evalTest(t, `(list 1 2 3 4)`,
		List(Integer(1), Integer(2), Integer(3), Integer(4)))
}

func TestTokenizer(t *testing.T) {
	evalTest(t, `
		(list 1 2 ;
		  3;
		  4)`,
		List(Integer(1),
			Integer(2),
			Integer(3),
			Integer(4)))
}

func TestAutoLispFunc(t *testing.T) {
	evalTest(t, `(setq c "a") c`, String("a"))

	evalTest(t, `
		(setq c "a")
		(defun f (a)
			(let ((c "b"))
				(+ a 1)
			)
		)
		(list (f 4) c)`,
		List(Integer(5), String("a")))

	evalTest(t, `
		(setq c "a")
		(defun f (a / c)
			(setq c "b")
			(+ a 1)
		)
		(list (f 4) c)`,
		List(Integer(5), String("a")))

	evalTest(t, `(let ((a 0)) (if T (setq a 1) (setq a 2)) a)`, Integer(1))
	evalTest(t, `(let ((x "1")) (if nil (setq x "2") (setq x "3")) x)`, String("3"))
}
