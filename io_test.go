package gmnlisp

import (
	"context"
	"os"
	"path/filepath"
	"testing"
)

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

func TestFileLength(t *testing.T) {
	testFile := filepath.Join(os.TempDir(), "testfile")
	fd, err := os.Create(testFile)
	if err != nil {
		t.Fatal(err.Error())
	}
	fd.WriteString("0123456789")
	fd.Close()
	defer os.Remove(testFile)

	lisp := New()
	lisp = lisp.Let(Variables{NewSymbol("fname"): String(testFile)})
	if eStr := lisp.Assert(`(file-length fname 8)`, Integer(10)); eStr != "" {
		t.Fatal(eStr)
	}
	if eStr := lisp.Assert(`(file-length fname 2)`, Integer(40)); eStr != "" {
		t.Fatal(eStr)
	}
}
