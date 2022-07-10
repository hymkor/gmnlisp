package gmnlisp

type Instance struct {
	globals map[string]Node
}

func New() *Instance {
	return &Instance{
		globals: map[string]Node{
			"T":           TrueValue,
			"nil":         NullValue,
			"print":       Function(CmdPrint),
			"prin1":       Function(CmdPrin1),
			"princ":       Function(CmdPrinc),
			"terpri":      Function(CmdTerpri),
			"quote":       Function(CmdQuote),
			"+":           Function(CmdPlus),
			"-":           Function(CmdMinus),
			"*":           Function(CmdMulti),
			"/":           Function(CmdDevide),
			"cons":        Function(CmdCons),
			"car":         Function(CmdCar),
			"cdr":         Function(CmdCdr),
			"atom":        Function(CmdAtom),
			"equal":       Function(CmdEqual),
			"lambda":      Function(CmdLambda),
			"progn":       Function(CmdProgn),
			"setq":        Function(CmdSetq),
			"defun":       Function(CmdDefun),
			"let":         Function(CmdLet),
			"cond":        Function(CmdCond),
			"return":      Function(CmdReturn),
			"return-from": Function(CmdReturnFrom),
			"block":       Function(CmdBlock),
			"truncate":    Function(CmdTruncate),
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
