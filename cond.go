package gmnlisp

import (
	"fmt"
)

func CmdCond(instance *Instance, node Node) (Node, error) {
	for HasValue(node) {
		cons, ok := node.(*Cons)
		if !ok {
			return nil, ErrExpectedCons
		}
		node = cons.Cdr

		conditionAndActions, ok := cons.Car.(*Cons)
		if !ok {
			return nil, fmt.Errorf("%w: %s", ErrExpectedCons, Node2String(cons.Car))
		}
		condition, err := conditionAndActions.GetCar().Eval(instance)
		if err != nil {
			return nil, err
		}
		if HasValue(condition) {
			result, err := progn(instance, conditionAndActions.Cdr)
			if err != nil {
				return result, err
			}
			return result, err
		}
	}
	return NullValue, nil
}
