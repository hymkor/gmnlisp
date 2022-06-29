package gommon

import (
	"regexp"
	"strconv"
	"strings"
)

var RxNumber = regexp.MustCompile("^[0-9]+$")

func readTokens(tokens []string) (Node, int) {
	if len(tokens) <= 0 {
		return &Null{}, 0
	}
	if tokens[0] == ")" {
		return &Null{}, 1
	}
	first := new(Cons)
	last := first
	i := 0
	for {
		if tokens[i] == "(" {
			i++
			car, n := readTokens(tokens[i:])
			last.Car = car
			i += n
		} else if RxNumber.MatchString(tokens[i]) {
			val, err := strconv.ParseInt(tokens[i], 10, 63)
			if err != nil {
				val = 0
			}
			last.Car = NodeInteger(val)
			i++
		} else {
			if strings.HasPrefix(tokens[i], "\"") {
				last.Car = NodeString(strings.Replace(tokens[i], "\"", "", -1))
			} else {
				last.Car = NodeSymbol(tokens[i])
			}
			i++
		}
		if i >= len(tokens) {
			last.Cdr = &Null{}
			return first, i
		}
		if tokens[i] == ")" {
			last.Cdr = &Null{}
			return first, i + 1
		}
		tmp := new(Cons)
		last.Cdr = tmp
		last = tmp
	}
}

func ReadTokens(tokens []string) Node {
	list, _ := readTokens(tokens)
	return list
}

func ReadString(s string) Node {
	return ReadTokens(StringToTokens(s))
}
