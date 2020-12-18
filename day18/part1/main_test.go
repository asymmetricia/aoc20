package main

import "testing"

func Test_eval(t *testing.T) {
	tests := []struct {
		arg          string
		wantResult   int
		wantConsumed int
	}{
		{
			arg:          "1+1",
			wantResult:   2,
			wantConsumed: 3,
		},
		{"1+2", 3, 3},
		{"1 + 2 * 3 + 4 * 5 + 6", 71, 21},
		{"2+4*9", 54, 5},
		{"(2+4*9)", 54, 7},
		{"((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2", 13632, 47},
	}
	for _, tt := range tests {
		t.Run(tt.arg, func(t *testing.T) {
			gotResult, gotConsumed := eval(tt.arg)
			if gotResult != tt.wantResult {
				t.Errorf("eval() gotResult = %v, want %v", gotResult, tt.wantResult)
			}
			if gotConsumed != tt.wantConsumed {
				t.Errorf("eval() gotConsumed = %v, want %v", gotConsumed, tt.wantConsumed)
			}
		})
	}
}
