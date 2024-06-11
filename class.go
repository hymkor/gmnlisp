package gmnlisp

import (
	"context"
	"errors"
	"fmt"
	"io"
	"strings"
)

type _SlotSpec struct {
	identifier Symbol
	reader     Symbol
	writer     Symbol
	accessor   Symbol
	boundp     Symbol
	initform   func() (Node, error)
	initarg    Symbol
}

func readSlotSpec(ctx context.Context, w *World, list Node) (*_SlotSpec, error) {
	cons, ok := list.(*Cons)
	if !ok {
		return nil, fmt.Errorf("[1] %w: %#v", ErrExpectedCons, list)
	}
	identifier, ok := cons.Car.(Symbol)
	if !ok {
		return nil, fmt.Errorf("[1] %w: %#v", ErrExpectedSymbol, cons.Car)
	}
	slotSpec := &_SlotSpec{identifier: identifier}

	list = cons.Cdr
	count := 1
	for IsSome(list) {
		count++
		keywordCons, ok := list.(*Cons)
		if !ok {
			return nil, fmt.Errorf("[%d][1] %w: %#v", count, ErrExpectedCons, list)
		}
		keyword, ok := keywordCons.Car.(Keyword)
		if !ok {
			return nil, fmt.Errorf("[%d][2] %w: %#v", count, ErrExpectedKeyword, keywordCons.Car)
		}
		valueCons, ok := keywordCons.Cdr.(*Cons)
		if !ok {
			return nil, fmt.Errorf("[%d][3] %w: %#v", count, ErrExpectedCons, keywordCons.Cdr)
		}
		var err error
		value := valueCons.Car
		switch keyword {
		case NewKeyword(":reader"):
			slotSpec.reader, ok = value.(Symbol)
		case NewKeyword(":writer"):
			slotSpec.writer, ok = value.(Symbol)
		case NewKeyword(":accessor"):
			slotSpec.accessor, ok = value.(Symbol)
		case NewKeyword(":boundp"):
			slotSpec.boundp, ok = value.(Symbol)
		case NewKeyword(":initform"):
			slotSpec.initform = func() (Node, error) { return value.Eval(ctx, w) }
		case NewKeyword(":initarg"):
			slotSpec.initarg, ok = value.(Symbol)
		}
		if err != nil {
			return nil, fmt.Errorf("[%d][4] %w: %#v", count, err, value)
		}
		if !ok {
			return nil, fmt.Errorf("[%d][5] Domain error: %#v", count, valueCons.Car)
		}
		list = valueCons.Cdr
	}
	return slotSpec, nil
}

type _Class struct {
	Name  Symbol
	Super map[Symbol]*_Class
	Slot  map[Symbol]*_SlotSpec
}

func cmdDefClass(ctx context.Context, w *World, args Node) (Node, error) {
	// (defclass class-name (sc-name*) (slot-spec*) class-opt*)

	// class-name
	_className, args, err := Shift(args)
	if err != nil {
		return nil, fmt.Errorf("[1] %w", err)
	}
	className, ok := _className.(Symbol)
	if !ok {
		return nil, fmt.Errorf("[1] %w: %#v", ErrExpectedSymbol, _className)
	}
	class := &_Class{
		Name:  className,
		Super: make(map[Symbol]*_Class),
		Slot:  make(map[Symbol]*_SlotSpec),
	}
	if w.shared.classes == nil {
		w.shared.classes = make(map[Symbol]*_Class)
	}
	if IsNone(args) {
		w.shared.classes[className] = class
		return className, nil
	}
	// (sc-name*) ... super class list
	_scNames, args, err := Shift(args)
	if err != nil {
		return nil, fmt.Errorf("[2] %w", err)
	}
	if IsSome(_scNames) {
		scNames, ok := _scNames.(*Cons)
		if !ok {
			return nil, fmt.Errorf("[2] %w: %#v", ErrExpectedCons, _scNames)
		}
		for p, ok := scNames.Car.(*Cons), true; ok && IsSome(p); p, ok = p.Cdr.(*Cons) {
			name, ok := p.Car.(Symbol)
			if !ok {
				return nil, fmt.Errorf("[2][%d] %w: %#v", 1+len(class.Super), ErrExpectedSymbol, p.Car)
			}
			class.Super[name] = nil
		}
	}
	if IsNone(args) {
		w.shared.classes[className] = class
		return className, nil
	}
	// (slot-spec*)
	_slotSpecs, args, err := Shift(args)
	if err != nil {
		return nil, err
	}
	slotSpecs, ok := _slotSpecs.(*Cons)
	if !ok {
		return nil, fmt.Errorf("[3] %w: %#v", ErrExpectedCons, _slotSpecs)
	}
	slotCount := 0
	for p, ok := slotSpecs, true; ok && IsSome(p); p, ok = p.Cdr.(*Cons) {
		slotCount++
		spec, err := readSlotSpec(ctx, w, p.Car)
		if err != nil {
			return nil, fmt.Errorf("[3][%d] %w", slotCount, err)
		}
		class.Slot[spec.identifier] = spec
	}
	if w.shared.classes == nil {
		w.shared.classes = make(map[Symbol]*_Class)
	}
	w.shared.classes[className] = class
	return className, nil
}

type _ClassInstance struct {
	ClassDefine *_Class
	Slot        map[Symbol]Node
}

func (c *_ClassInstance) Eval(ctx context.Context, w *World) (Node, error) {
	return _NullType{}, errors.New("not callable")
}

func (c *_ClassInstance) Equals(Node, EqlMode) bool {
	return false
}

func (c *_ClassInstance) PrintTo(w io.Writer, mode PrintMode) (int, error) {
	n, err := c.ClassDefine.Name.PrintTo(w, mode)
	if err != nil {
		return n, err
	}
	delim := []byte{'{'}
	for key, val := range c.Slot {
		n1, err := w.Write(delim)
		n += n1
		if err != nil {
			return n, err
		}
		n1, err = key.PrintTo(w, mode)
		n += n1
		if err != nil {
			return n, err
		}
		n1, err = w.Write([]byte{':'})
		n += n1
		if err != nil {
			return n, err
		}
		n1, err = val.PrintTo(w, mode)
		n += n1
		if err != nil {
			return n, err
		}
		delim = []byte{','}
	}
	n1, err := io.WriteString(w, "}")
	n += n1
	return n, err
}

func (c *_ClassInstance) String() string {
	var buffer strings.Builder
	c.PrintTo(&buffer, PRINC)
	return buffer.String()
}

func (c *_ClassInstance) GoString() string {
	var buffer strings.Builder
	c.PrintTo(&buffer, PRINT)
	return buffer.String()
}

func cmdCreate(ctx context.Context, w *World, args Node) (Node, error) {
	_className, args, err := Shift(args)
	if err != nil {
		return nil, err
	}
	className, ok := _className.(Symbol)
	if !ok {
		return nil, ErrExpectedSymbol
	}
	classDef, ok := w.shared.classes[className]
	if !ok {
		return nil, fmt.Errorf("class %v undefined", className)
	}
	this := &_ClassInstance{
		ClassDefine: classDef,
		Slot:        map[Symbol]Node{},
	}
	for IsSome(args) {
		var _initArg Node
		_initArg, args, err = w.ShiftAndEvalCar(ctx, args)
		if err != nil {
			return nil, err
		}
		initArg, ok := _initArg.(Symbol)
		if !ok {
			return nil, ErrExpectedSymbol
		}
		var initVal Node
		initVal, args, err = w.ShiftAndEvalCar(ctx, args)
		if err != nil {
			return nil, err
		}
		for name, slot1 := range classDef.Slot {
			if slot1.initarg == initArg {
				this.Slot[name] = initVal
			}
		}
	}
	for name, slot1 := range classDef.Slot {
		if _, ok := this.Slot[name]; !ok && slot1.initform != nil {
			var err error
			this.Slot[name], err = slot1.initform()
			if err != nil {
				return nil, err
			}
		}
	}
	return this, nil
}
