package gmnlisp

import (
	"testing"
)

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
