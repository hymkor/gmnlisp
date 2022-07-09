package gmnlisp

import (
	"fmt"
)

func CmdCond(node Node) (Node, error) {
	for !IsNull(node) {
		cons, ok := node.(*Cons)
		if !ok {
			return nil, ErrExpectedCons
		}
		node = cons.Cdr

		conditionAndActions, ok := cons.Car.(*Cons)
		if !ok {
			return nil, fmt.Errorf("%w: %s", ErrExpectedCons, Node2String(cons.Car))
		}
		condition, err := conditionAndActions.GetCar().Eval()
		if err != nil {
			return nil, err
		}
		if !IsNull(condition) {
			result, err := progn(conditionAndActions.Cdr)
			if err != nil {
				return result, err
			}
			return result, err
		}
	}
	return NullValue, nil
}
