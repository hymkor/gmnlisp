package gommon

var globals map[string]Node

func init() {
	globals = map[string]Node{
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
	}
}