package common

import . "github.com/hymkor/gmnlisp"

// This code is generated by lsp2go.lsp
var embedFunctions = map[Symbol]Node{
	NewSymbol("set-nth"):    &LispString{S: `(lambda (newvalue Z L) (set-elt newvalue L Z))`},
	NewSymbol("set-nthcdr"): &LispString{S: `(lambda (newvalue z source) (let ((s source)) (while s (setq z (1- z)) (if (zerop z) (set-cdr newvalue s)) (setq s (cdr s))) source))`},
	NewSymbol("set-cadr"):   &LispString{S: `(lambda (newvalue L) (set-car newvalue (cdr L)))`},
	NewSymbol("set-caddr"):  &LispString{S: `(lambda (newvalue L) (set-car newvalue (cdr (cdr L))))`},
	NewSymbol("set-cadddr"): &LispString{S: `(lambda (newvalue L) (set-car newvalue (cdr (cdr (cdr L)))))`},
	NewSymbol("set-cddr"):   &LispString{S: `(lambda (newvalue L) (set-cdr newvalue (cdr L)))`},
	NewSymbol("set-cdddr"):  &LispString{S: `(lambda (newvalue L) (set-cdr newvalue (cdr (cdr L))))`},
}