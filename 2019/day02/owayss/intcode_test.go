package main

import (
	"fmt"
	"reflect"
	"testing"
)

func TestEval(t *testing.T) {
	type input struct {
		prog  []int64
		start int
	}
	type output struct {
		prog []int64
		err  error
	}
	var tests = []struct {
		in   input
		want output
	}{
		{
			in: input{
				prog:  []int64{1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50},
				start: 0,
			},
			want: output{
				prog: []int64{1, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50},
				err:  nil,
			},
		},
	}

	for _, tt := range tests {
		testname := fmt.Sprintf("prog: %v\nstart: %d\n", tt.in.prog, tt.in.start)
		t.Run(testname, func(t *testing.T) {
			err := eval(tt.in.prog, tt.in.start)
			if err != tt.want.err {
				t.Errorf("got %v, want %v", err, tt.want.err)
			}
			if !reflect.DeepEqual(tt.in.prog, tt.want.prog) {
				t.Errorf("got %v, want %v", tt.in.prog, tt.want.prog)
			}
		})
	}
}

func TestOutput(t *testing.T) {
	type out struct {
		result int64
		err    error
	}
	var tests = []struct {
		in   []int64
		want out
	}{
		{
			in: []int64{1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50},
			want: out{
				result: 3500,
				err:    nil,
			},
		},
	}

	for _, tt := range tests {
		testname := fmt.Sprintf("prog: %v\n", tt.in)
		t.Run(testname, func(t *testing.T) {
			res, err := output(tt.in)
			if err != tt.want.err {
				t.Errorf("got %v, want %v", err, tt.want.err)
			}
			if res != tt.want.result {
				t.Errorf("got %d, want %d", res, tt.want.result)
			}
		})
	}
}
