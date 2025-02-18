package gmnlisp

import "testing"

func TestDiv(t *testing.T) {
	testcases := [][3]int{
		{12, 3, 4},
		{14, 3, 4},
		{-12, 3, -4},
		{-14, 3, -5},
		{12, -3, -4},
		{14, -3, -5},
		{-12, -3, 4},
		{-14, -3, 4},
	}
	for _, tc := range testcases {
		if result := div(tc[0], tc[1]); result != tc[2] {
			t.Fatalf("%d/%d = %d (expected %d)", tc[0], tc[1], result, tc[2])
		}
	}
}

func TestMod(t *testing.T) {
	testcases := [][3]int{
		{12, 3, 0},
		{7, 247, 7},
		{247, 7, 2},
		{14, 3, 2},
		{-12, -3, 0},
		{-14, 3, 1},
		{12, -3, 0},
		{14, -3, -1},
		{-12, -3, 0},
		{-14, -3, -2},
	}
	for _, tc := range testcases {
		if result := mod(tc[0], tc[1]); result != tc[2] {
			t.Fatalf("%d %% %d = %d (expected %d)", tc[0], tc[1], result, tc[2])
		}
	}
}
