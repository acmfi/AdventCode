package main

import (
	"bufio"
	"bytes"
	"errors"
	"fmt"
	"log"
	"os"
	"strconv"
)

var (
	HCF      = errors.New("halt and catch fire")
	initProg []int64
)

// load initial program
func init() {
	file, err := os.Open("input")
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	splitComma := func(data []byte, atEOF bool) (advance int, token []byte, err error) {
		dataLen := len(data)

		// Return nothing if at end of file and no data passed
		if atEOF && dataLen == 0 {
			return 0, nil, nil
		}

		// Find next separator and return token
		if i := bytes.IndexByte(data, ','); i >= 0 {
			return i + 1, data[0:i], nil
		}

		// If we're at EOF, we have a final, non-terminated line. Return it.
		if atEOF {
			return dataLen, data, nil
		}

		// Request more data.
		return 0, nil, nil
	}
	scanner.Split(splitComma)

	for scanner.Scan() {
		n, err := strconv.ParseInt(scanner.Text(), 10, 64)
		if err != nil {
			fmt.Println(err)
		}
		initProg = append(initProg, n)
	}
}

func eval(prog []int64, start int) error {
	opCode := prog[start]
	leftOperand := prog[prog[start+1]]
	rightOperand := prog[prog[start+2]]
	resIdx := prog[start+3]
	var res int64
	switch opCode {
	case 1:
		res = leftOperand + rightOperand
	case 2:
		res = leftOperand * rightOperand
	case 99:
		return HCF
	}
	prog[resIdx] = res
	return nil
}

func output(prog []int64) (int64, error) {
	for end := 4; end < len(prog); end += 4 {
		if err := eval(prog, end-4); err != nil {
			return prog[0], err
		}
	}
	return prog[0], nil
}

func main() {
	prog := make([]int64, len(initProg), cap(initProg))
	for i := 0; i < 100; i++ {
		for j := 0; j < 100; j++ {
			// reset prog memory to initial state before HCL
			copy(prog, initProg)

			prog[1] = int64(i)
			prog[2] = int64(j)
			if res, _ := output(prog); res == 19690720 {
				fmt.Printf("i=%d, j=%d, 100*i+j=%d\n", i, j, (100*i)+j)
				return
			}
		}
	}
	fmt.Println(output(prog))
}
