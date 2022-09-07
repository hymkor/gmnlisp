package common

import (
	"os"
	"testing"

	. "github.com/hymkor/gmnlisp"
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
