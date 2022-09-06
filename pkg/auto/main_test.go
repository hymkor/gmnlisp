package auto

import (
	"os"
	"testing"

	"github.com/hymkor/gmnlisp"
)

func assertEqual(t *testing.T, equation string, expect gmnlisp.Node) {
	w := gmnlisp.New().Let(Functions)
	if e := w.Assert(equation, expect); e != "" {
		t.Fatal(e)
	}
}

func TestString(t *testing.T) {
	assertEqual(t, `(string-append "12345" "67890")`, gmnlisp.String("1234567890"))
	assertEqual(t, `(strcase "abcdef")`, gmnlisp.String("ABCDEF"))
}

func TestIo(t *testing.T) {
	assertEqual(t, `
		(let (fd line)
			(setq fd (open (quote "../../LICENSE") (quote "r"))
				  line (read-line fd))
			(close fd)
			line
		)`, gmnlisp.String("MIT License"))

	assertEqual(t, `
		(let ((fd (open (quote "temp.txt") "w")))
			(format fd "hogehoge~%")
			(close fd))
		(let (fd line)
			(setq fd (open "temp.txt" "r")
			      line (read-line fd))
			(close fd)
			line)`, gmnlisp.String("hogehoge"))

	os.Remove("temp.txt")

	// not exist file test
	assertEqual(t, `(open "temp.txt" "r")`, gmnlisp.Null)
}

func TestReadFromString(t *testing.T) {
	assertEqual(t, `(read-from-string "(1 2 3)")`, gmnlisp.List(gmnlisp.Integer(1), gmnlisp.Integer(2), gmnlisp.Integer(3)))
}

func TestForeach(t *testing.T) {
	assertEqual(t, `(let ((x 0)(i 0))
						(foreach i (list 1 (+ 1 1) (* 1 3) 4 5)
							(setq x (+ x i))
						)
						x
					)`, gmnlisp.Integer(15))

}
