package gmnlisp

import (
	"testing"
)

func TestDistanceClass(t *testing.T) {
	type testCaseT struct {
		distance int
		ok       bool
		class1   Class
		class2   Class
	}

	testCases := []*testCaseT{
		&testCaseT{0, true, ObjectClass, ObjectClass},
		&testCaseT{1, true, integerClass, numberClass},
		&testCaseT{0, false, numberClass, integerClass},
	}
	for _, tc := range testCases {
		distance, ok := distanceClass(tc.class1, tc.class2)
		if ok != tc.ok {
			t.Fatalf("%s and %s: expect %v, but %#v", tc.class1.String(), tc.class2.String(), tc.ok, ok)
		}
		if ok {
			if distance != tc.distance {
				t.Fatalf("%s and %s: expect %#v, but %#v", tc.class1.String(), tc.class2.String(), tc.distance, distance)
			}
		}
	}
}
