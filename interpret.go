package gommon

func Interpret(code string) (Node, error) {
	compiled, err := ReadString(code)
	if err != nil {
		return nil, err
	}
	var result Node
	for _, c := range compiled {
		c, err = macroQuote(c)
		if err != nil {
			return c, err
		}
		result, err = c.Eval()
		if err != nil {
			return result, err
		}
	}
	return result, err
}