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
	class, ok := _class.(*_StandardClass)
	if !ok {
		t.Fatal("testclass not found")
	}
	slot, ok := class.Slot[NewSymbol("f")]
	if !ok {
		t.Fatal("slot f not found")
	}
	if len(slot.reader) < 1 || slot.reader[0] != NewSymbol("R") {
		t.Fatal("reader not found")
	}
	if len(slot.writer) < 1 || slot.writer[0] != NewSymbol("W") {
		t.Fatal("writer not found")
	}
	if len(slot.accessor) < 1 || slot.accessor[0] != NewSymbol("A") {
		t.Fatal("accessor not found")
	}
	if len(slot.initarg) < 1 || slot.initarg[0] != NewSymbol("I") {
		t.Fatal("initarg not found")
	}
	if val, err := slot.initform(); err != nil {
		t.Fatal(err.Error())
	} else if val != Integer(3) {
		t.Fatalf("iniform is wrong: %#v", val)
	}
}
