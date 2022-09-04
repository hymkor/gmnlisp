package common

import (
	"testing"

	. "github.com/hymkor/gmnlisp"
)

func TestWrite(t *testing.T){
	assertEqual(t, `
		(let ((buffer (create-string-output-stream)))
			(write "123" :stream buffer)
			(get-output-stream-string buffer)
		)`,String(`"123"`))
}

func TestWriteLine(t *testing.T){
	assertEqual(t, `
		(let ((buffer (create-string-output-stream)))
			(write-line "123" buffer)
			(get-output-stream-string buffer)
		)`,String("123\n"))
}

