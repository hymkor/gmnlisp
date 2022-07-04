package gommon

import (
	"errors"
)

func macroQuote(node Node) (Node, error) {
	cons, ok := node.(*Cons)
	if !ok {
		return node, nil
	}
	// normal pair
	if !cons.GetCar().Equals(NodeSymbol("'")) {
		car, err := macroQuote(cons.Car)
		if err != nil {
			return nil, err
		}
		cdr, err := macroQuote(cons.Cdr)
		if err != nil {
			return nil, err
		}
		return &Cons{Car: car, Cdr: cdr}, nil
	}

	// Find single quotation mark
	cons, ok = cons.Cdr.(*Cons)
	if !ok {
		return nil, errors.New("not a cons after single quotation")
	}
	quoted, err := macroQuote(cons.Car)
	if err != nil {
		return nil, err
	}
	rest, err := macroQuote(cons.Cdr)
	if err != nil {
		return nil, err
	}
	return &Cons{
		Car: &Cons{
			Car: NodeSymbol("quote"),
			Cdr: &Cons{
				Car: quoted,
				Cdr: nil,
			},
		},
		Cdr: rest,
	}, nil
}
