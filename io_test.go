package gmnlisp

import (
	"context"
	"os"
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

func TestReadLine(t *testing.T) {
	assertEqual(t, `
		(let*
			((lf (create-string 1 #\newline))
			 (s (string-append "1" lf "2" lf "3"))
			 (r (create-string-input-stream s)))
			(and
				(equalp (read-line r nil "EOF") "1")
				(equalp (read-line r nil "EOF") "2")
				(equalp (read-line r nil "EOF") "3")
				(equalp (read-line r nil "EOF") "EOF")
			)
		)
	`, True)
}

func TestOpenOutpuFile(t *testing.T) {
	w := New()
	_, err := w.Interpret(context.TODO(), `
		(let ((w (open-output-file "hogehoge")))
			(unwind-protect
				(format w "hogehoge~%")
				(close w)
			)
		)
	`)
	if err != nil {
		t.Fatal(err.Error())
	}
	defer os.Remove("hogehoge")

	output, err := os.ReadFile("hogehoge")
	if err != nil {
		t.Fatal(err.Error())
	}
	if o := string(output); o != "hogehoge\n" {
		t.Fatalf(`expect "hogehoge" but "%s"`, o)
	}
}

func TestWithOpenInputFile(t *testing.T) {
	assertEqual(t, `
		(with-open-input-file (fd "LICENSE")
			(read-line fd))`, String("MIT License"))
}

func TestOpenOutputFile(t *testing.T) {
	w := New()
	_, err := w.Interpret(context.TODO(), `
		(with-open-output-file (w "hogehoge")
			(format w "hogehoge~%")
		)
	`)
	if err != nil {
		t.Fatal(err.Error())
	}
	defer os.Remove("hogehoge")

	output, err := os.ReadFile("hogehoge")
	if err != nil {
		t.Fatal(err.Error())
	}
	if o := string(output); o != "hogehoge\n" {
		t.Fatalf(`expect "hogehoge" but "%s"`, o)
	}
}

func TestProbeFile(t *testing.T) {
	assertEqual(t, `(probe-file ".")`, True)
	assertEqual(t, `(probe-file "notexist.lsp")`, Null)
}
