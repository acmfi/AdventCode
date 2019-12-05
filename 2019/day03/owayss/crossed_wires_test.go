package main

import (
	"fmt"
	"reflect"
	"testing"
)

func TestExpandWire(t *testing.T) {
	var tests = []struct {
		in   []string
		want []*point
	}{
		{
			in: []string{"R8", "U5", "L5", "D3"},
			want: []*point{
				&point{1, 0}, &point{2, 0}, &point{3, 0}, &point{4, 0}, &point{5, 0}, &point{6, 0}, &point{7, 0}, &point{8, 0}, &point{8, 1}, &point{8, 2}, &point{8, 3}, &point{8, 4}, &point{8, 5}, &point{7, 5}, &point{6, 5}, &point{5, 5}, &point{4, 5}, &point{3, 5}, &point{3, 4}, &point{3, 3}, &point{3, 2},
			},
		},
		{
			in: []string{"U7", "R6", "D4", "L4"},
			want: []*point{
				&point{0, 1}, &point{0, 2}, &point{0, 3}, &point{0, 4}, &point{0, 5}, &point{0, 6}, &point{0, 7}, &point{1, 7}, &point{2, 7}, &point{3, 7}, &point{4, 7}, &point{5, 7}, &point{6, 7}, &point{6, 6}, &point{6, 5}, &point{6, 4}, &point{6, 3}, &point{5, 3}, &point{4, 3}, &point{3, 3}, &point{2, 3},
			},
		},
	}

	for _, tt := range tests {
		testname := fmt.Sprintf("in: %v\n", tt.in)
		t.Run(testname, func(t *testing.T) {
			wire := expandWire(tt.in)
			if !reflect.DeepEqual(wire, tt.want) {
				t.Errorf("got %v\nwant %v", wire, tt.want)
			}
		})
	}
}

func TestManhattan(t *testing.T) {
	type input struct {
		p, q *point
	}
	var tests = []struct {
		in   input
		want int
	}{
		{
			in: input{
				p: &point{0, 0},
				q: &point{3, 3},
			},
			want: 6,
		},
		{
			in: input{
				p: &point{0, 0},
				q: &point{6, 5},
			},
			want: 11,
		},
	}

	for _, tt := range tests {
		testname := fmt.Sprintf("in: %v\n", tt.in)
		t.Run(testname, func(t *testing.T) {
			distance := (*point).manhattan(tt.in.p, tt.in.q)
			if distance != tt.want {
				t.Errorf("got %d want %d", distance, tt.want)
			}
		})
	}
}
