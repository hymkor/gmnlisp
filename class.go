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

type _Getter struct {
	Symbol
	class map[Symbol]func(*_ClassInstance) (Node, error)
}

func (acc *_Getter) findClass(class *_Class) func(*_ClassInstance) (Node, error) {
	if f, ok := acc.class[class.Symbol]; ok {
		return f
	}
	for _, super := range class.Super {
		if f := acc.findClass(super); f != nil {
			return f
		}
	}
	return nil
}

func (acc *_Getter) Call(ctx context.Context, w *World, node Node) (Node, error) {
	_this, _, err := w.ShiftAndEvalCar(ctx, node)
	if err != nil {
		return nil, err
	}
	instance, ok := _this.(*_ClassInstance)
	if !ok {
		return nil, errors.New("Expect Class Instance")
	}
	f := acc.findClass(instance._Class)
	if f == nil {
		return nil, fmt.Errorf("reciever %v not found in %v", acc.Symbol.String(), instance._Class.Symbol.String())
	}
	return f(instance)
}

type _Setter struct {
	Symbol
	class map[Symbol]func(*_ClassInstance, Node)
}

func (acc *_Setter) findClass(class *_Class) func(*_ClassInstance, Node) {
	if f := acc.class[class.Symbol]; f != nil {
		return f
	}
	for _, super := range class.Super {
		if f := acc.findClass(super); f != nil {
			return f
		}
	}
	return nil
}

func (acc *_Setter) Call(ctx context.Context, w *World, node Node) (Node, error) {
	value, node, err := w.ShiftAndEvalCar(ctx, node)
	if err != nil {
		return nil, err
	}
	_instance, _, err := w.ShiftAndEvalCar(ctx, node)
	if err != nil {
		return nil, err
	}
	instance, ok := _instance.(*_ClassInstance)
	if !ok {
		return nil, errors.New("Expect Class Instance")
	}
	f := acc.findClass(instance._Class)
	if f == nil {
		return nil, errors.New("reciever not found")
	}
	f(instance, value)
	return acc, nil
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
			// println("accessor=", slotSpec.accessor.String())
		case NewKeyword(":boundp"):
			slotSpec.boundp, ok = value.(Symbol)
		case NewKeyword(":initform"):
			slotSpec.initform = func() (Node, error) { return value.Eval(ctx, w) }
		case NewKeyword(":initarg"):
			slotSpec.initarg, ok = value.(Symbol)
		default:
			return nil, fmt.Errorf("invalid keyword %v", keyword)
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
	Symbol
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
		Symbol: className,
		Super:  make(map[Symbol]*_Class),
		Slot:   make(map[Symbol]*_SlotSpec),
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
		// println(scNames.GoString())
		for p, ok := scNames, true; ok && IsSome(p); p, ok = p.Cdr.(*Cons) {
			name, ok := p.Car.(Symbol)
			if !ok {
				return nil, fmt.Errorf("[2][%d] %w: %#v", 1+len(class.Super), ErrExpectedSymbol, p.Car)
			}
			//println("class", className, "inherits", name)
			class.Super[name] = w.shared.classes[name]
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

		if IsSome(spec.accessor) {
			getter := func(this *_ClassInstance) (Node, error) {
				return this.Slot[spec.identifier], nil
			}
			if _acc, err := w.Get(spec.accessor); err == nil {
				// println("accessor is found", spec.accessor.String())
				if acc, ok := _acc.(*_Getter); ok {
					acc.class[className] = getter
				} else {
					return nil, fmt.Errorf("%v: already defined as not accessor", spec.accessor)
				}
			} else {
				// println("accessor not found", spec.accessor.String())
				w.DefineGlobal(spec.accessor, &_Getter{
					Symbol: spec.accessor,
					class: map[Symbol]func(*_ClassInstance) (Node, error){
						className: getter,
					},
				})
			}
			setter := func(this *_ClassInstance, value Node) {
				this.Slot[spec.identifier] = value
			}
			setterName := NewSymbol("set-" + spec.accessor.String())
			if _acc, err := w.Get(setterName); err == nil {
				// println("accessor is found", spec.accessor.String())
				if acc, ok := _acc.(*_Setter); ok {
					acc.class[className] = setter
				} else {
					return nil, fmt.Errorf("%v: already defined as not accessor", spec.accessor)
				}
			} else {
				// println("accessor not found", spec.accessor.String())
				w.DefineGlobal(setterName, &_Setter{
					Symbol: setterName,
					class: map[Symbol]func(*_ClassInstance, Node){
						className: setter,
					},
				})
			}
		}
	}
	if w.shared.classes == nil {
		w.shared.classes = make(map[Symbol]*_Class)
	}
	w.shared.classes[className] = class
	return className, nil
}

type _ClassInstance struct {
	*_Class
	Slot map[Symbol]Node
}

func (c *_ClassInstance) PrintTo(w io.Writer, mode PrintMode) (int, error) {
	n, err := c._Class.Symbol.PrintTo(w, mode)
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

func (instance *_ClassInstance) callInitForm(classDef *_Class) error {
	for _, super := range classDef.Super {
		if err := instance.callInitForm(super); err != nil {
			return err
		}
	}
	for name, slot1 := range classDef.Slot {
		if _, ok := instance.Slot[name]; !ok && slot1.initform != nil {
			var err error
			instance.Slot[name], err = slot1.initform()
			if err != nil {
				return err
			}
		}
	}
	return nil
}

func (instance *_ClassInstance) callInitArg(classDef *_Class, initArg Symbol, initVal Node) bool {
	for name, slot1 := range classDef.Slot {
		if slot1.initarg == initArg {
			instance.Slot[name] = initVal
			return true
		}
	}
	for _, super := range classDef.Super {
		if instance.callInitArg(super, initArg, initVal) {
			return true
		}
	}
	return false
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
		_Class: classDef,
		Slot:   map[Symbol]Node{},
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
		this.callInitArg(classDef, initArg, initVal)
	}
	return this, this.callInitForm(classDef)
}
