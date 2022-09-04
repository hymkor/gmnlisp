package gmnlisp

import (
	"os"
	"testing"
)

func TestWithOpenFile(t *testing.T) {
	assertEqual(t, `
		(let (output)
			(with-open-file (w "datafile" :direction :output)
				(format w "hogehoge~%")
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

func TestRead(t *testing.T) {
	assertEqual(t, `
		(let ((r (create-string-input-stream "1 \"ahaha\" 3")))
			(and
				(equalp (read r nil "EOF") 1)
				(equalp (read r nil "EOF") "ahaha")
				(equalp (read r nil "EOF") 3)
				(equalp (read r nil "EOF") "EOF"))
		)
	`, True)

}
