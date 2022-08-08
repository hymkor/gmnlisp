package gmnlisp

import (
	"testing"
)

func TestList(t *testing.T) {
	assertEqual(t, "(car '(1 2))", Integer(1))
	assertEqual(t, "(car '(1 . 2))", Integer(1))
	assertEqual(t, "(cdr '(1 . 2))", Integer(2))
	assertEqual(t, "(cdr '(1 2))", List(Integer(2)))

	assertEqual(t, `(list 1 2 3 4)`,
		List(Integer(1), Integer(2), Integer(3), Integer(4)))

	assertEqual(t, `(cadr '(1 2 3))`, Integer(2))
	assertEqual(t, `(caddr '(1 2 3 4 5 ))`, Integer(3))
	assertEqual(t, `(cadddr '(1 2 3 4 5))`, Integer(4))
	assertEqual(t, `(cddr '(1 2 3 4 5))`,
		List(Integer(3), Integer(4), Integer(5)))
	assertEqual(t, `(cdddr '(1 2 3 4 5))`,
		List(Integer(4), Integer(5)))

	assertEqual(t, "(cons 1 2)", &Cons{Car: Integer(1), Cdr: Integer(2)})

	assertEqual(t, `(listp ())`, True)
	assertEqual(t, `(listp 1)`, Null)
	assertEqual(t, `(listp '(1 2 3))`, True)

	assertEqual(t, `(let ((collection '((a . 1) (b . 2) (c . 3))))
			(assoc 'a collection))`,
		&Cons{Car: Symbol("a"), Cdr: Integer(1)})

	assertEqual(t, `(nth 2 '(10 20 30 40))`, Integer(30))
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

func TestAppend(t *testing.T) {
	assertEqual(t, `(append '(1 2) '(3 4))`, List(Integer(1), Integer(2), Integer(3), Integer(4)))
	assertEqual(t, `(append '(1 2) '(3 4) '(5 6))`,
		List(Integer(1), Integer(2), Integer(3), Integer(4), Integer(5), Integer(6)))
	assertEqual(t, `(append '() '(1 2) '(3 4))`,
		List(Integer(1), Integer(2), Integer(3), Integer(4)))

	// append do not destruct original not last list.
	assertEqual(t, `(let ((x '(1 2 3)))
		(append x '(4 5 6))
		x)`, List(Integer(1), Integer(2), Integer(3)))

	// apend destruct the last list only
	assertEqual(t, `
			(let* ((part1 (list 1 2 3))
				(part2 (list 4 5 6))
				(part3 (list 7 8 9))
				(all (append part1 part2 part3)))
					(setf (nth 1 all) "aaa")
					(setf (nth 4 all) "bbb")
					(setf (nth 7 all) "ccc")
					(and
						(equal part1 '(1 2 3))
						(equal part2 '(4 5 6))
						(equal part3 '(7 "ccc" 9))))`, True)
}

func TestLast(t *testing.T) {
	assertEqual(t, `(last '(1 2 3 4))`, Integer(4))
	assertEqual(t, `(last '())`, Null)
}

func TestDestoroy(t *testing.T) {
	assertEqual(t, `(let ((c '("A" . "D"))) (replaca c "X") c)`,
		&Cons{Car: String("X"), Cdr: String("D")})

	assertEqual(t, `(let ((c '("A" . "D"))) (replacd c "X") c)`,
		&Cons{Car: String("A"), Cdr: String("X")})
}

func TestFirst(t *testing.T) {
	assertEqual(t, `(first '(1 2 3 4))`, Integer(1))
	assertEqual(t, `(second '(1 2 3 4))`, Integer(2))
	assertEqual(t, `(third '(1 2 3 4))`, Integer(3))
	assertEqual(t, `(rest '(1 2 3 4))`, List(Integer(2), Integer(3), Integer(4)))
}
