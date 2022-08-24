package gmnlisp

import (
	"os"
	"testing"
)

func TestIo(t *testing.T) {
	assertEqual(t, `
		(let (fd line)
			(setq fd (open (quote "LICENSE") (quote "r"))
				  line (read-line fd))
			(close fd)
			line
		)`, String("MIT License"))

	assertEqual(t, `
		(let ((fd (open (quote "temp.txt") "w")))
			(write-line "hogehoge" fd)
			(close fd))
		(let (fd line)
			(setq fd (open "temp.txt" "r")
			      line (read-line fd))
			(close fd)
			line)`, String("hogehoge"))

	os.Remove("temp.txt")

	// not exist file test
	assertEqual(t, `(open "temp.txt" "r")`, Null)
}

func TestWithOpenFile(t *testing.T) {
	assertEqual(t, `
		(let (output)
			(with-open-file (w "datafile" :direction :output)
				(write-line "hogehoge" w)
			)
			(with-open-file (r "datafile")
				(setq output (read-line r))
			)
			output)`, String("hogehoge"))

	assertEqual(t, `
		(with-open-file (r "hogehogehoge" :if-does-not-exist "ERROR")
			(if (eql r "ERROR")
				"FAILURE"
				"SUCCESS"
			)
		)`, String("FAILURE"))

	os.Remove("datafile")
}

func TestCreateStringOutputStream(t *testing.T) {
	assertEqual(t, `
		(let ((str (create-string-output-stream)))
			(format str "hello")
			(format str "world")
			(get-output-stream-string str))`,
		String("helloworld"))
}
