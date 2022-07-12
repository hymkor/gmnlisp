package gmnlisp

import (
	"fmt"
	"io"
	"os"
)

type Instance struct {
	globals map[string]Node
	Stdout  io.Writer
}

func New() *Instance {
	return &Instance{
		Stdout: os.Stdout,
		globals: map[string]Node{
			"T":           True,
			"nil":         Null,
			"print":       Function(cmdPrint),
			"prin1":       Function(cmdPrin1),
			"princ":       Function(cmdPrinc),
			"terpri":      Function(cmdTerpri),
			"quote":       Function(cmdQuote),
			"+":           Function(cmdPlus),
			"-":           Function(cmdMinus),
			"*":           Function(cmdMulti),
			"/":           Function(cmdDevide),
			"<":           Function(cmdLessThan),
			">":           Function(cmdGreaterThan),
			"<=":          Function(cmdLessOrEqual),
			">=":          Function(cmdGreaterOrEqual),
			"=":           Function(cmdEqualOp),
			"equalp":      Function(cmdEqualOp),
			"cons":        Function(cmdCons),
			"car":         Function(cmdCar),
			"cdr":         Function(cmdCdr),
			"atom":        Function(cmdAtom),
			"equal":       Function(cmdEqual),
			"lambda":      Function(cmdLambda),
			"progn":       Function(cmdProgn),
			"setq":        Function(cmdSetq),
			"defun":       Function(cmdDefun),
			"let":         Function(cmdLet),
			"cond":        Function(cmdCond),
			"return":      Function(cmdReturn),
			"return-from": Function(cmdReturnFrom),
			"block":       Function(cmdBlock),
			"truncate":    Function(cmdTruncate),
		},
	}
}

func (ins *Instance) ShiftAndEvalCar(node Node) (Node, Node, error) {
	cons, ok := node.(*Cons)
	if !ok {
		return nil, nil, ErrTooFewOrTooManyArguments
	}
	value, err := cons.GetCar().Eval(ins)
	if err != nil {
		return nil, nil, err
	}
	return value, cons.Cdr, nil
}

func (ins *Instance) Inject(this Node, f func(left, right Node) (Node, error)) (Node, error) {
	result, rest, err := ins.ShiftAndEvalCar(this)
	if err != nil {
		return nil, err
	}
	for HasValue(rest) {
		var next Node
		var err error

		next, rest, err = ins.ShiftAndEvalCar(rest)
		if err != nil {
			return nil, err
		}
		result, err = f(result, next)
		if err != nil {
			return nil, err
		}
	}
	return result, nil
}

func forEachWithoutEval(this Node, f func(Node) error) error {
	for HasValue(this) {
		cons, ok := this.(*Cons)
		if !ok {
			return fmt.Errorf("%w (%s)", ErrExpectedCons, toString(this))
		}
		if err := f(cons.Car); err != nil {
			return err
		}
		this = cons.Cdr
	}
	return nil
}

func (ins *Instance) Interpret(code string) (Node, error) {
	compiled, err := ReadString(code)
	if err != nil {
		return nil, err
	}
	return compiled.Eval(ins)
}

func (ins *Instance) InterpretBytes(code []byte) (Node, error) {
	compiled, err := ReadBytes(code)
	if err != nil {
		return nil, err
	}
	return compiled.Eval(ins)
}
