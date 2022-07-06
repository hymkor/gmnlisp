package gommon

import (
	"fmt"
)

func CmdCond(node Node) (Node, error) {
	for !IsNull(node) {
		cons, ok := node.(*Cons)
		if !ok {
			return nil, fmt.Errorf("cond: %w", ErrExpectedCons)
		}
		node = cons.Cdr

		conditionAndActions, ok := cons.Car.(*Cons)
		if !ok {
			return nil, fmt.Errorf("cond: %w: %s", ErrExpectedCons, Node2String(cons.Car))
		}
		condition, err := conditionAndActions.GetCar().Eval()
		if err != nil {
			return nil, fmt.Errorf("cond: %w", err)
		}
		if !IsNull(condition) {
			return CmdProgn(conditionAndActions.Cdr)
		}
	}
	return &Null{}, nil
}
