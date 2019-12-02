package main

import (
	"fmt"
	"testing"
)

func TestNeededFuel(t *testing.T) {
	var tests = []struct {
		input uint64
		want  uint64
	}{
		{12, 2},
		{14, 2},
		{1969, 654},
		{100756, 33583},
	}

	for _, tt := range tests {
		testname := fmt.Sprintf("mass: %d", tt.input)
		t.Run(testname, func(t *testing.T) {
			ans := neededFuel(tt.input)
			if ans != tt.want {
				t.Errorf("got %d, want %d", ans, tt.want)
			}
		})
	}
}
