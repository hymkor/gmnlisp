package gmnlisp

type Instance struct {
	globals map[string]Node
}

func New() *Instance {
	return &Instance{
		globals: map[string]Node{
			"T":           TrueValue,
			"nil":         NullValue,
			"print":       Function(cmdPrint),
			"prin1":       Function(cmdPrin1),
			"princ":       Function(cmdPrinc),
			"terpri":      Function(cmdTerpri),
			"quote":       Function(cmdQuote),
			"+":           Function(cmdPlus),
			"-":           Function(cmdMinus),
			"*":           Function(cmdMulti),
			"/":           Function(cmdDevide),
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
