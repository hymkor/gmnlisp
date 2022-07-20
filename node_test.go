package gmnlisp

import (
	"testing"
)

func TestString(t *testing.T) {
	assertEqual(t, `(strcat "12345" "67890")`, String("1234567890"))
}
