package gmnlisp

import (
	"context"
	"fmt"
)

type _SlotSpec struct {
	reader   Symbol
	writer   Symbol
	accessor Symbol
	boundp   Symbol
	initform Node
	initarg  Symbol
}

func readSlotSpec(ctx context.Context, w *World, list Node) (Symbol, *_SlotSpec, error) {
	cons, ok := list.(*Cons)
	if !ok {
		return -1, nil, fmt.Errorf("[1] %w: %#v", ErrExpectedCons, list)
	}
	identifier, ok := cons.Car.(Symbol)
	if !ok {
		return -1, nil, fmt.Errorf("[1] %w: %#v", ErrExpectedSymbol, cons.Car)
	}
	slotSpec := &_SlotSpec{}

	list = cons.Cdr
	count := 1
	for IsSome(list) {
		count++
		keywordCons, ok := list.(*Cons)
		if !ok {
			return -1, nil, fmt.Errorf("[%d][1] %w: %#v", count, ErrExpectedCons, list)
		}
		keyword, ok := keywordCons.Car.(Keyword)
		if !ok {
			return -1, nil, fmt.Errorf("[%d][2] %w: %#v", count, ErrExpectedKeyword, keywordCons.Car)
		}
		valueCons, ok := keywordCons.Cdr.(*Cons)
		if !ok {
			return -1, nil, fmt.Errorf("[%d][3] %w: %#v", count, ErrExpectedCons, keywordCons.Cdr)
		}
		var err error
		value := valueCons.Car
		switch keyword {
		case Keyword(":reader"):
			slotSpec.reader, ok = value.(Symbol)
		case Keyword(":writer"):
			slotSpec.writer, ok = value.(Symbol)
		case Keyword(":accessor"):
			slotSpec.accessor, ok = value.(Symbol)
		case Keyword(":boundp"):
			slotSpec.boundp, ok = value.(Symbol)
		case Keyword(":initform"):
			slotSpec.initform, err = value.Eval(ctx, w)
		case Keyword(":initarg"):
			slotSpec.initarg, ok = value.(Symbol)
		}
		if err != nil {
			return -1, nil, fmt.Errorf("[%d][4] %w: %#v", count, err, value)
		}
		if !ok {
			return -1, nil, fmt.Errorf("[%d][5] Domain error: %#v", count, valueCons.Car)
		}
		list = valueCons.Cdr
	}
	return identifier, slotSpec, nil
}

type _Class struct {
	// Name string
	Super map[Symbol]*_Class
	Slot  map[Symbol]*_SlotSpec
}

var classes map[Symbol]*_Class

func cmdDefClass(ctx context.Context, w *World, args Node) (Node, error) {
	_className, args, err := Shift(args)
	if err != nil {
		return nil, fmt.Errorf("[1] %w", err)
	}
	className, ok := _className.(Symbol)
	if !ok {
		return nil, fmt.Errorf("[1] %w: %#v", ErrExpectedSymbol, _className)
	}
	_scNames, args, err := Shift(args)
	if err != nil {
		return nil, fmt.Errorf("[2] %w", err)
	}
	class := &_Class{
		Super: make(map[Symbol]*_Class),
		Slot:  make(map[Symbol]*_SlotSpec),
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
		symbol, spec, err := readSlotSpec(ctx, w, p.Car)
		if err != nil {
			return nil, fmt.Errorf("[3][%d] %w", slotCount, err)
		}
		class.Slot[symbol] = spec
	}
	if w.shared.classes == nil {
		w.shared.classes = make(map[Symbol]*_Class)
	}
	w.shared.classes[className] = class
	return className, nil
}
