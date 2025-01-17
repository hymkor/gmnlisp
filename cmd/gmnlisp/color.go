package main

import (
	"unicode/utf8"

	"github.com/nyaosorg/go-readline-ny"
)

type kakkoColor struct {
	last   string
	result [][][]int
}

func (K *kakkoColor) update(s string) {
	if K.last == s {
		return
	}
	K.last = s
	for i := 0; i < len(K.result); i++ {
		if K.result[i] != nil {
			K.result[i] = K.result[i][:0]
		}
	}
	depth := 0
	quote := false
	pos := 0
	for len(s) > 0 {
		r, siz := utf8.DecodeRuneInString(s)
		s = s[siz:]

		if r == '"' {
			quote = !quote
		} else if r == '\\' && len(s) > 0 {
			_, siz2 := utf8.DecodeRuneInString(s)
			s = s[siz2:]
			pos += siz2
		} else if !quote && r == '(' {
			K.result[depth%len(K.result)] = append(K.result[depth%len(K.result)], []int{pos, pos + siz})
			depth++
		} else if !quote && r == ')' && depth > 0 {
			depth--
			K.result[depth%len(K.result)] = append(K.result[depth%len(K.result)], []int{pos, pos + siz})
		}
		pos += siz
	}
}

type kakkoColorLevel struct {
	*kakkoColor
	n int
}

func (K kakkoColorLevel) FindAllStringIndex(s string, _ int) [][]int {
	K.update(s)
	return K.kakkoColor.result[K.n]
}

func (K *kakkoColor) newKakkoLevel(seq string) readline.Highlight {
	rc := readline.Highlight{
		Pattern:  kakkoColorLevel{n: len(K.result), kakkoColor: K},
		Sequence: seq,
	}
	K.result = append(K.result, nil)
	return rc
}
