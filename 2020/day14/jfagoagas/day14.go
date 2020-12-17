package main

import (
	"fmt"
	"io/ioutil"
	"os"
	"strconv"
	"strings"
)

func main() {
	file, _ := ioutil.ReadFile(os.Args[1])
	input := strings.Split((string(file)), "\n")
	memory := map[uint64]uint64{}
	mask := ""
	for _, value := range input {
		// Update mask
		if strings.HasPrefix(value, "mask = ") {
			mask = strings.TrimPrefix(value, "mask = ")
			continue
		}
		// Scan values
		position := 0
		oldValue := 0
		fmt.Sscanf(value, "mem[%d] = %d", &position, &oldValue)
		binaryValue := strconv.FormatInt(int64(oldValue), 2)
		newValue := ""
		maskValue := ""
		for b := 0; b < len(mask)-len(binaryValue); b++ {
			newValue += "0"
		}
		newValue += binaryValue
		//fmt.Println(oldValue, binaryValue)
		for i := range mask {
			if mask[i] == 'X' {
				maskValue += string(newValue[i])
			} else if mask[i] == '1' {
				maskValue += "1"
			} else {
				maskValue += "0"
			}

		}
		value, _ := strconv.ParseInt(maskValue, 2, 64)
		memory[uint64(position)] = uint64(value)
	}
	sum := 0
	for _, value := range memory {
		sum += int(value)
	}
	fmt.Printf("Day 14\nPart 1: %d\n", sum)
}
