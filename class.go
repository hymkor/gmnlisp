package gmnlisp

import (
	"context"
	"fmt"
	"io"
	"strings"
)

type _SlotSpec struct {
	identifier Symbol
	reader     []Symbol
	writer     []Symbol
	accessor   []Symbol
	boundp     []Symbol
	initform   func() (Node, error)
	initarg    []Symbol
}

func newGetter(class Class, slotName Symbol) *_Method {
	return &_Method{
		types: []Class{class},
		method: func(ctx context.Context, w *World, node []Node) (Node, error) {
			rec, ok := node[0].(*_Receiver)
			if !ok {
				return nil, fmt.Errorf("%v: %w", node[0], ErrExpectedClass)
			}
			if val, ok := rec.Slot[slotName]; ok {
				return val, nil
			}
			return Null, nil
		},
	}
}

func newSetter(class Class, slotName Symbol) *_Method {
	return &_Method{
		types: []Class{objectClass, class},
		method: func(ctx context.Context, w *World, node []Node) (Node, error) {
			rec, ok := node[1].(*_Receiver)
			if !ok {
				return nil, fmt.Errorf("%v: %w", node[1], ErrExpectedClass)
			}
			rec.Slot[slotName] = node[0]
			return True, nil
		},
	}
}

func newBoundp(class Class, slotName Symbol) *_Method {
	return &_Method{
		types: []Class{class},
		method: func(ctx context.Context, w *World, node []Node) (Node, error) {
			rec, ok := node[0].(*_Receiver)
			if !ok {
				return nil, fmt.Errorf("%v: %w", node[0], ErrExpectedClass)
			}
			if _, ok := rec.Slot[slotName]; ok {
				return True, nil
			}
			return Null, nil
		},
	}
}

func registerMethod(w *World, methodName Symbol, class Class, method *_Method) error {
	if _acc, err := w.Get(methodName); err == nil {
		if gen, ok := _acc.(*_Generic); ok {
			gen.methods = append(gen.methods, method)
		} else {
			return fmt.Errorf("%v: already defined as not method", methodName)
		}
	} else {
		w.DefineGlobal(methodName, &_Generic{
			Symbol:  methodName,
			argc:    len(method.types),
			rest:    method.restType != nil,
			methods: []*_Method{method},
		})
	}
	return nil
}

var (
	kwReader   = NewKeyword(":reader")
	kwWriter   = NewKeyword(":writer")
	kwAccessor = NewKeyword(":accessor")
	kwBoundp   = NewKeyword(":boundp")
	kwInitForm = NewKeyword(":initform")
	kwInitArg  = NewKeyword(":initarg")
)

func readSlotSpec(ctx context.Context, w *World, list Node) (*_SlotSpec, error) {
	_identifier, list, err := Shift(list)
	if err != nil {
		return nil, err
	}
	identifier, ok := _identifier.(Symbol)
	if !ok {
		return nil, fmt.Errorf("%v: %w", _identifier, ErrExpectedSymbol)
	}
	slotSpec := &_SlotSpec{identifier: identifier}
	count := 1
	for IsSome(list) {
		var _keyword Node
		var err error
		var value Node

		count++
		_keyword, list, err = Shift(list)
		if err != nil {
			return nil, err
		}
		keyword, ok := _keyword.(Keyword)
		if !ok {
			return nil, fmt.Errorf("%v: %w", _keyword, ErrExpectedKeyword)
		}
		value, list, err = Shift(list)
		if err != nil {
			return nil, err
		}
		switch keyword {
		case kwReader:
			if v, ok := value.(Symbol); ok {
				slotSpec.reader = append(slotSpec.reader, v)
			} else {
				return nil, fmt.Errorf(":reader:  %w", ErrExpectedSymbol)
			}
		case kwWriter:
			if v, ok := value.(Symbol); ok {
				slotSpec.writer = append(slotSpec.writer, v)
			} else {
				return nil, fmt.Errorf(":writer: %w", ErrExpectedSymbol)
			}
		case kwAccessor:
			if v, ok := value.(Symbol); ok {
				slotSpec.accessor = append(slotSpec.accessor, v)
			} else {
				return nil, fmt.Errorf(":accessor: %w", ErrExpectedSymbol)
			}
		case kwBoundp:
			if v, ok := value.(Symbol); ok {
				slotSpec.boundp = append(slotSpec.boundp, v)
			} else {
				return nil, fmt.Errorf(":boundp: %w", ErrExpectedSymbol)
			}
		case kwInitForm:
			slotSpec.initform = func() (Node, error) { return value.Eval(ctx, w) }
		case kwInitArg:
			if v, ok := value.(Symbol); ok {
				slotSpec.initarg = append(slotSpec.initarg, v)
			} else {
				return nil, fmt.Errorf(":initarg: %w", ErrExpectedSymbol)
			}
		default:
			return nil, fmt.Errorf("invalid keyword %v", keyword)
		}
	}
	return slotSpec, nil
}

type _UserClass struct {
	serial int
	Symbol
	Super []Class
	Slot  map[Symbol]*_SlotSpec
}

func (u *_UserClass) Equals(_other Node, _ EqlMode) bool {
	if u.serial == 0 {
		panic("class serial is zero")
	}
	other, ok := _other.(*_UserClass)
	if !ok {
		return false
	}
	return u.serial == other.serial
}

func (class1 *_UserClass) InheritP(class2 Class) bool {
	for _, s := range class1.Super {
		if s.Equals(class2, STRICT) {
			return true
		}
		if s.InheritP(class2) {
			return true
		}
	}
	return false
}

func (c *_UserClass) Eval(ctx context.Context, w *World) (Node, error) {
	return c, nil
}

func (c *_UserClass) String() string {
	return "{*_UserClass}" + c.Symbol.String()
}

func (c *_UserClass) Name() Symbol {
	return c.Symbol
}

// subClassP returns true when class1 is one of the sub-classes of class2
func (class1 *_UserClass) subClassP(class2 *_UserClass) bool {
	for _, super := range class1.Super {
		if super.Name() == class2.Name() {
			return true
		}
		if uc, ok := super.(*_UserClass); ok && uc.subClassP(class2) {
			return true
		}
	}
	return false
}

func (c *_UserClass) InstanceP(obj Node) bool {
	class := obj.ClassOf()
	userClass, ok := class.(*_UserClass)
	if !ok {
		// println("InstanceP: NG1")
		return false
	}
	if userClass.Symbol.Equals(c.Symbol, STRICT) {
		return true
	}
	// println("InstanceP: NG2")
	return userClass.subClassP(c)
}

func (c *_UserClass) Create() Node {
	return &_Receiver{
		_UserClass: c,
		Slot:       make(map[Symbol]Node),
	}
}

var symInitializeObject = NewSymbol("initialize-object")

var classCounter = 0

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
	classCounter++
	class := &_UserClass{
		serial: classCounter,
		Symbol: className,
		Slot:   make(map[Symbol]*_SlotSpec),
	}
	if IsNone(args) {
		w.DefineGlobal(className, class)
		return className, nil
	}
	// (sc-name*) ... super class list
	_scNames, args, err := Shift(args)
	if err != nil {
		return nil, fmt.Errorf("[2] %w", err)
	}
	for IsSome(_scNames) {
		var _super Node
		_super, _scNames, err = w.ShiftAndEvalCar(ctx, _scNames)
		if err != nil {
			return nil, err
		}
		super, ok := _super.(Class)
		if !ok {
			return nil, fmt.Errorf("%v: %w", _super, ErrExpectedClass)
		}
		class.Super = append(class.Super, super)
	}
	if IsNone(args) {
		w.DefineGlobal(className, class)
		return className, nil
	}
	// (slot-spec*)
	_slotSpecs, args, err := Shift(args)
	if err != nil {
		return nil, err
	}
	slotCount := 0
	for IsSome(_slotSpecs) {
		slotCount++
		var slot1 Node
		slot1, _slotSpecs, err = Shift(_slotSpecs)
		if err != nil {
			return nil, err
		}
		spec, err := readSlotSpec(ctx, w, slot1)
		if err != nil {
			return nil, fmt.Errorf("[3][%d] %w", slotCount, err)
		}
		class.Slot[spec.identifier] = spec

		getter := newGetter(class, spec.identifier)
		setter := newSetter(class, spec.identifier)
		for _, f := range spec.boundp {
			boundp := newBoundp(class, spec.identifier)
			if err := registerMethod(w, f, class, boundp); err != nil {
				return nil, fmt.Errorf("boundp: %w", err)
			}
		}
		for _, f := range spec.reader {
			if err := registerMethod(w, f, class, getter); err != nil {
				return nil, err
			}
		}
		for _, f := range spec.writer {
			if err := registerMethod(w, f, class, setter); err != nil {
				return nil, err
			}
		}
		for _, f := range spec.accessor {
			if err := registerMethod(w, f, class, getter); err != nil {
				return nil, err
			}
			setterName := NewSymbol("set-" + f.String())
			if err := registerMethod(w, setterName, class, setter); err != nil {
				return nil, err
			}
		}
	}
	w.DefineGlobal(className, class)

	registerMethod(w, symInitializeObject, class, &_Method{
		restType: objectClass,
		types:    []Class{class},
		method:   defaultInitializeObject,
	})
	return className, nil
}

type _Receiver struct {
	*_UserClass
	Slot map[Symbol]Node
}

func (r *_Receiver) Eval(ctx context.Context, w *World) (Node, error) {
	return r, nil
}

func (c *_Receiver) ClassOf() Class {
	return c._UserClass
}

func (c *_Receiver) PrintTo(w io.Writer, mode PrintMode) (int, error) {
	n, err := c._UserClass.Symbol.PrintTo(w, mode)
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

func (c *_Receiver) String() string {
	var buffer strings.Builder
	c.PrintTo(&buffer, PRINC)
	return buffer.String()
}

func (c *_Receiver) GoString() string {
	var buffer strings.Builder
	c.PrintTo(&buffer, PRINT)
	return buffer.String()
}

func (reciever *_Receiver) callInitForm(classDef *_UserClass) error {
	for _, super := range classDef.Super {
		if uc, ok := super.(*_UserClass); ok {
			if err := reciever.callInitForm(uc); err != nil {
				return err
			}
		}
	}
	for name, slot1 := range classDef.Slot {
		if _, ok := reciever.Slot[name]; !ok && slot1.initform != nil {
			var err error
			reciever.Slot[name], err = slot1.initform()
			if err != nil {
				return err
			}
		}
	}
	return nil
}

func (reciever *_Receiver) callInitArg(classDef *_UserClass, initArg Symbol, initVal Node) bool {
	for name, slot := range classDef.Slot {
		for _, slotInitArg := range slot.initarg {
			if slotInitArg == initArg {
				reciever.Slot[name] = initVal
				return true
			}
		}
	}
	for _, super := range classDef.Super {
		if superUc, ok := super.(*_UserClass); ok {
			if reciever.callInitArg(superUc, initArg, initVal) {
				return true
			}
		}
	}
	return false
}

type Uneval struct {
	Node
}

func (u Uneval) Eval(ctx context.Context, w *World) (Node, error) {
	return u.Node, nil
}

func cmdCreate(ctx context.Context, w *World, args Node) (Node, error) {
	_class, args, err := w.ShiftAndEvalCar(ctx, args)
	if err != nil {
		return nil, err
	}
	class, ok := _class.(Class)
	if !ok {
		return nil, fmt.Errorf("%v: %w", _class, ErrExpectedClass)
	}
	_gen, err := w.Get(symInitializeObject)
	if err != nil {
		return nil, err
	}
	gen, ok := _gen.(*_Generic)
	if !ok {
		return nil, ErrExpectedGeneric
	}
	rec := class.Create()
	if _, ok := rec.(*_Receiver); ok {
		return gen.Call(ctx, w, &Cons{Car: Uneval{Node: rec}, Cdr: args})
	}
	return rec, nil
}

func defaultInitializeObject(ctx context.Context, w *World, args []Node) (Node, error) {
	_this, args := args[0], args[1:]
	this, ok := _this.(*_Receiver)
	if !ok {
		if len(args) > 0 {
			return nil, fmt.Errorf("%s does not have slot", _this.String())
		}
		return _this, nil
	}
	for len(args) > 0 {
		var _initArg Node
		_initArg, args = args[0], args[1:]
		initArg, ok := _initArg.(Symbol)
		if !ok {
			return nil, fmt.Errorf("defaultInitializeObject: initArg: %w: %v", ErrExpectedSymbol, _initArg)
		}
		if len(args) <= 0 {
			return nil, ErrTooFewArguments
		}
		var initVal Node
		initVal, args = args[0], args[1:]
		this.callInitArg(this._UserClass, initArg, initVal)
	}
	return this, this.callInitForm(this._UserClass)
}

func defInstanceP(ctx context.Context, w *World, node Node) (Node, error) {
	value, node, err := w.ShiftAndEvalCar(ctx, node)
	if err != nil {
		return nil, err
	}
	_class, node, err := w.ShiftAndEvalCar(ctx, node)
	if err != nil {
		return nil, err
	}
	class, ok := _class.(Class)
	if !ok {
		return nil, fmt.Errorf("%v: %w", _class, ErrExpectedClass)
	}
	if IsSome(node) {
		return nil, ErrTooManyArguments
	}
	if class.InstanceP(value) {
		return True, nil
	}
	return Null, nil
}

func cmdClass(ctx context.Context, w *World, node Node) (Node, error) {
	value, node, err := w.ShiftAndEvalCar(ctx, node)
	if err != nil {
		return nil, err
	}
	if IsSome(node) {
		return nil, ErrTooManyArguments
	}
	_, ok := value.(Class)
	if !ok {
		return nil, fmt.Errorf("%v: %w", value, ErrExpectedClass)
	}
	return value, nil
}

func funSubClassP(ctx context.Context, w *World, node []Node) (Node, error) {
	class1, ok := node[0].(Class)
	if !ok {
		return nil, fmt.Errorf("%v: %w", node[0], ErrExpectedClass)
	}
	class2, ok := node[1].(Class)
	if !ok {
		return nil, fmt.Errorf("%v: %w", node[1], ErrExpectedClass)
	}
	if class1.InheritP(class2) {
		return True, nil
	}
	return Null, nil
}
