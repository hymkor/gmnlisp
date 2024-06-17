package gmnlisp

import (
	"context"
	"testing"
)

func TestDefClass(t *testing.T) {
	w := New()
	todo := context.TODO()
	_, err := w.Interpret(todo, `
		(defclass
			testclass
			()
			((f :reader R
				:writer W
				:accessor A
				:boundp B
				:initform (+ 1 2)
				:initarg I)))`)
	if err != nil {
		t.Fatal(err.Error())
	}
	_class, err := w.Interpret(todo, "testclass")
	if err != nil {
		t.Fatal(err.Error())
	}
	class, ok := _class.(*_UserClass)
	if !ok {
		t.Fatal("testclass not found")
	}
	slot, ok := class.Slot[NewSymbol("f")]
	if !ok {
		t.Fatal("slot f not found")
	}
	if slot.reader != NewSymbol("R") {
		t.Fatal("reader not found")
	}
	if slot.writer != NewSymbol("W") {
		t.Fatal("writer not found")
	}
	if slot.accessor != NewSymbol("A") {
		t.Fatal("accessor not found")
	}
	if slot.initarg != NewSymbol("I") {
		t.Fatal("initarg not found")
	}
	if val, err := slot.initform(); err != nil {
		t.Fatal(err.Error())
	} else if val != Integer(3) {
		t.Fatalf("iniform is wrong: %#v", val)
	}
}
