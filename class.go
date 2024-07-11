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
			rec, ok := node[0].(*_StandardObject)
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
			rec, ok := node[1].(*_StandardObject)
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
			rec, ok := node[0].(*_StandardObject)
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
	if _acc, err := w.GetFunc(methodName); err == nil {
		if gen, ok := _acc.(*_Generic); ok {
			gen.methods = append(gen.methods, method)
		} else {
			return fmt.Errorf("%v: already defined as not method", methodName)
		}
	} else {
		w.defun.Set(methodName, &_Generic{
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
	identifier, err := ExpectClass[Symbol](ctx, w, _identifier)
	if err != nil {
		return nil, err
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
		keyword, err := ExpectClass[Keyword](ctx, w, _keyword)
		if err != nil {
			return nil, err
		}
		value, list, err = Shift(list)
		if err != nil {
			return nil, err
		}
		switch keyword {
		case kwReader:
			if v, err := ExpectClass[Symbol](ctx, w, value); err == nil {
				slotSpec.reader = append(slotSpec.reader, v)
			} else {
				return nil, fmt.Errorf("%#v: %w", keyword.String(), err)
			}
		case kwWriter:
			if v, err := ExpectClass[Symbol](ctx, w, value); err == nil {
				slotSpec.writer = append(slotSpec.writer, v)
			} else {
				return nil, fmt.Errorf("%#v: %w", keyword.String(), err)
			}
		case kwAccessor:
			if v, err := ExpectClass[Symbol](ctx, w, value); err == nil {
				slotSpec.accessor = append(slotSpec.accessor, v)
			} else {
				return nil, fmt.Errorf("%#v: %w", keyword.String(), err)
			}
		case kwBoundp:
			if v, err := ExpectClass[Symbol](ctx, w, value); err == nil {
				slotSpec.boundp = append(slotSpec.boundp, v)
			} else {
				return nil, fmt.Errorf("%#v: %w", keyword.String(), err)
			}
		case kwInitForm:
			slotSpec.initform = func() (Node, error) { return w.Eval(ctx, value) }
		case kwInitArg:
			if v, err := ExpectClass[Symbol](ctx, w, value); err == nil {
				slotSpec.initarg = append(slotSpec.initarg, v)
			} else {
				return nil, fmt.Errorf("%#v: %w", keyword.String(), err)
			}
		default:
			return nil, fmt.Errorf("invalid keyword %v", keyword)
		}
	}
	return slotSpec, nil
}

type _StandardClass struct {
	serial int
	Symbol Symbol
	Super  []Class
	Slot   map[Symbol]*_SlotSpec
}

// standardClass can not be created with registerNewBuilInClass
// because it does not inherit <built-in-class>.
var standardClass = &_BuiltInClass{
	name: NewSymbol("<standard-class>"),
	instanceP: func(n Node) bool {
		_, ok := n.(*_StandardClass)
		return ok
	},
	create: func() Node {
		classCounter++
		return &_StandardClass{}
	},
	super: []Class{objectClass},
}

func (u *_StandardClass) ClassOf() Class {
	return standardClass
}

func (u *_StandardClass) Equals(_other Node, _ EqlMode) bool {
	if u.serial == 0 {
		panic("class serial is zero")
	}
	other, ok := _other.(*_StandardClass)
	if !ok {
		return false
	}
	return u.serial == other.serial
}

func (class1 *_StandardClass) InheritP(class2 Class) bool {
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

func (c *_StandardClass) String() string {
	return "{*_UserClass}" + c.Symbol.String()
}

func (c *_StandardClass) GoString() string {
	return "{*_UserClass}" + c.Symbol.String()
}

func (c *_StandardClass) PrintTo(w io.Writer, mode PrintMode) (int, error) {
	return io.WriteString(w, c.Symbol.String())
}

func (c *_StandardClass) Name() Symbol {
	return c.Symbol
}

// subClassP returns true when class1 is one of the sub-classes of class2
func (class1 *_StandardClass) subClassP(class2 *_StandardClass) bool {
	for _, super := range class1.Super {
		if super.Name() == class2.Name() {
			return true
		}
		if uc, ok := super.(*_StandardClass); ok && uc.subClassP(class2) {
			return true
		}
	}
	return false
}

func (c *_StandardClass) InstanceP(obj Node) bool {
	class := obj.ClassOf()
	userClass, ok := class.(*_StandardClass)
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

func (c *_StandardClass) Create() Node {
	return &_StandardObject{
		_StandardClass: c,
		Slot:           make(map[Symbol]Node),
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
	className, err := ExpectClass[Symbol](ctx, w, _className)
	if err != nil {
		return nil, err
	}
	classCounter++
	class := &_StandardClass{
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
	_slotSpecs, _, err := Shift(args)
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

type _StandardObject struct {
	_StandardClass *_StandardClass
	Slot           map[Symbol]Node
}

func (r *_StandardObject) Equals(o Node, m EqlMode) bool {
	other, ok := o.(*_StandardObject)
	if !ok {
		return false
	}
	if m == STRICT {
		return r == other
	}
	if !r._StandardClass.Equals(other._StandardClass, m) {
		return false
	}
	if len(r.Slot) != len(other.Slot) {
		return false
	}
	for key, val := range r.Slot {
		if !val.Equals(other.Slot[key], m) {
			return false
		}
	}
	return true
}

func (c *_StandardObject) ClassOf() Class {
	return c._StandardClass
}

func (c *_StandardObject) PrintTo(w io.Writer, mode PrintMode) (int, error) {
	n, err := c._StandardClass.Symbol.PrintTo(w, mode)
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
		n1, err = tryPrintTo(w, val, mode)
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

func (c *_StandardObject) String() string {
	var buffer strings.Builder
	c.PrintTo(&buffer, PRINC)
	return buffer.String()
}

func (c *_StandardObject) GoString() string {
	var buffer strings.Builder
	c.PrintTo(&buffer, PRINT)
	return buffer.String()
}

func (reciever *_StandardObject) callInitForm(classDef *_StandardClass) error {
	for _, super := range classDef.Super {
		if uc, ok := super.(*_StandardClass); ok {
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

func (reciever *_StandardObject) callInitArg(classDef *_StandardClass, initArg Symbol, initVal Node) bool {
	for name, slot := range classDef.Slot {
		for _, slotInitArg := range slot.initarg {
			if slotInitArg == initArg {
				reciever.Slot[name] = initVal
				return true
			}
		}
	}
	for _, super := range classDef.Super {
		if superUc, ok := super.(*_StandardClass); ok {
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

func UnevalList(list ...Node) Node {
	var result Node = Null
	for i := len(list) - 1; i >= 0; i-- {
		result = &Cons{
			Car: Uneval{Node: list[i]},
			Cdr: result,
		}
	}
	return result
}

func funCreate(ctx context.Context, w *World, args []Node) (Node, error) {
	class, err := ExpectClass[Class](ctx, w, args[0])
	if err != nil {
		return nil, err
	}
	_gen, err := w.GetFunc(symInitializeObject)
	if err != nil {
		return nil, err
	}
	gen, err := ExpectGeneric(_gen)
	if err != nil {
		return nil, err
	}
	rec := class.Create()
	if _, ok := rec.(*_StandardObject); ok {
		newargs := append([]Node{rec}, args[1:]...)
		return gen.Call(ctx, w, UnevalList(newargs...))
	}
	return rec, nil
}

func defaultInitializeObject(ctx context.Context, w *World, args []Node) (Node, error) {
	_this, args := args[0], args[1:]
	this, ok := _this.(*_StandardObject)
	if !ok {
		if len(args) > 0 {
			return nil, fmt.Errorf("%s does not have slot", _this.String())
		}
		return _this, nil
	}
	for len(args) > 0 {
		var _initArg Node
		_initArg, args = args[0], args[1:]
		initArg, err := ExpectClass[Symbol](ctx, w, _initArg)
		if !ok {
			return nil, fmt.Errorf("defaultInitializeObject: initArg: %w: %v", err, _initArg)
		}
		if len(args) <= 0 {
			return nil, ErrTooFewArguments
		}
		var initVal Node
		initVal, args = args[0], args[1:]
		this.callInitArg(this._StandardClass, initArg, initVal)
	}
	return this, this.callInitForm(this._StandardClass)
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
