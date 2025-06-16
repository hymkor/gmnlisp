package subst

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

func TestList(t *testing.T) {
	assertEqual(t, `(nthcdr 2 '(10 20 30 40))`, List(Integer(30), Integer(40)))
}

func TestSubst(t *testing.T) {
	assertEqual(t, `
	(let ((m '(("X" . 1) ("Y" . 2) ("Z" . 4))))
		(subst (cons "X" 7) (assoc "X" m) m))`,
		List(
			&Cons{Car: String("X"), Cdr: Integer(7)},
			&Cons{Car: String("Y"), Cdr: Integer(2)},
			&Cons{Car: String("Z"), Cdr: Integer(4)}))

	// subst does not destoroy original list
	assertEqual(t, `
	(let ((m '(("X" . 1) ("Y" . 2) ("Z" . 4))))
		(subst (cons "X" 7) (assoc "X" m) m)
		m)`,
		List(
			&Cons{Car: String("X"), Cdr: Integer(1)},
			&Cons{Car: String("Y"), Cdr: Integer(2)},
			&Cons{Car: String("Z"), Cdr: Integer(4)}))
}
