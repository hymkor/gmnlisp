package gmnlisp

import (
	"os"
	"testing"
)

func TestIo(t *testing.T) {
	assertEqual(t, `
		(let (fd line)
			(setq fd (open "LICENSE" "r")
				  line (read-line fd))
			(close fd)
			line
		)`, String("MIT License"))

	assertEqual(t, `
		(let ((fd (open "temp.txt" "w")))
			(write-line "hogehoge" fd)
			(close fd))
		(let (fd line)
			(setq fd (open "temp.txt" "r")
			      line (read-line fd))
			(close fd)
			line)`, String("hogehoge"))

	os.Remove("temp.txt")
}
