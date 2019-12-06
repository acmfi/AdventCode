package main

import (
	"fmt"
	"io/ioutil"
	"math"
	"os"
	"strconv"
	"strings"
)

var intcode []int = nil

func main() {
	// Part 1
	readFile()
	calcVal(intcode)
}

func calcVal(input []int) {
	var avance int = 0
	for i := 0; i < len(input); i += avance {
		op := input[i]
		opcode := math.Mod(float64(op), 100)
		mode1 := math.Mod((float64(op) / (math.Pow(10, 2))), 10)
		mode2 := math.Mod((float64(op) / (math.Pow(10, 3))), 10)
		mode3 := math.Mod((float64(op) / (math.Pow(10, 4))), 10)
		mode1 = math.Round(mode1)
		mode2 = math.Round(mode2)
		mode3 = math.Round(mode3)
		if opcode == 1 {
			if mode1 == 0 && mode2 == 0 {
				input[input[i+3]] = input[input[i+1]] + input[input[i+2]]
			} else if mode1 == 1 && mode2 == 1 {
				input[input[i+3]] = input[i+1] + input[i+2]
			} else if mode1 == 1 && mode2 == 0 {
				input[input[i+3]] = input[i+1] + input[input[i+2]]
			} else if mode1 == 0 && mode2 == 1 {
				input[input[i+3]] = input[input[i+1]] + input[i+2]
			} else {
				break
			}
			avance = 4
		} else if opcode == 2 {
			if mode1 == 0 && mode2 == 0 {
				input[input[i+3]] = input[input[i+1]] * input[input[i+2]]
			} else if mode1 == 1 && mode2 == 1 {
				input[input[i+3]] = input[i+1] * input[i+2]
			} else if mode1 == 1 && mode2 == 0 {
				input[input[i+3]] = input[i+1] * input[input[i+2]]
			} else if mode1 == 0 && mode2 == 1 {
				input[input[i+3]] = input[input[i+1]] * input[i+2]
			} else {
				break
			}
			avance = 4
		} else if opcode == 3 {
			fmt.Printf("Please write the input value: ")
			fmt.Scanf("%d", &input[input[i+1]])
			avance = 2
		} else if opcode == 4 {
			if mode1 == 0 {
				fmt.Printf("%d\n", input[input[i+1]])
			} else if mode1 == 1 {
				fmt.Printf("%d\n", input[i+1])
			}
			avance = 2
		} else if opcode == 5 {
			if mode1 == 1 {
				if input[i+1] != 0 {
					if mode2 == 1 {
						i = input[i+2]
						continue
					} else if mode2 == 0 {
						i = input[input[i+2]]
						continue
					} else {
					}
				}
			} else if mode1 == 0 {
				if input[input[i+1]] != 0 {
					if mode2 == 1 {
						i = input[i+2]
						continue
					} else if mode2 == 0 {
						i = input[input[i+2]]
						continue
					} else {
					}
				}
			} else {
			}
		} else if opcode == 6 {
			if mode1 == 1 {
				if input[i+1] == 0 {
					if mode2 == 1 {
						i = input[i+2]
						continue
					} else if mode2 == 0 {
						i = input[input[i+2]]
						continue
					}
				}
			} else if mode1 == 0 {
				if input[input[i+1]] == 0 {
					if mode2 == 1 {
						i = input[i+2]
						continue
					} else if mode2 == 0 {
						i = input[input[i+2]]
						continue
					}
				}
			}

		} else if opcode == 7 {
			if mode1 == 0 && mode2 == 0 {
				if input[input[i+1]] < input[input[i+2]] {
					input[input[i+3]] = 1
				} else {
					input[input[i+3]] = 0
				}
			} else if mode1 == 1 && mode2 == 1 {
				if input[i+1] < input[i+2] {
					input[input[i+3]] = 1
				} else {
					input[input[i+3]] = 0
				}
			} else if mode1 == 0 && mode2 == 1 {
				if input[input[i+1]] < input[i+2] {
					input[input[i+3]] = 1
				} else {
					input[input[i+3]] = 0
				}
			} else if mode1 == 1 && mode2 == 0 {
				if input[i+1] < input[input[i+2]] {
					input[input[i+3]] = 1
				} else {
					input[input[i+3]] = 0
				}
			}

		} else if opcode == 8 {
			if mode1 == 0 && mode2 == 0 {
				if input[input[i+1]] == input[input[i+2]] {
					input[input[i+3]] = 1
				} else {
					input[input[i+3]] = 0
				}
			} else if mode1 == 1 && mode2 == 1 {
				if input[i+1] == input[i+2] {
					input[input[i+3]] = 1
				} else {
					input[input[i+3]] = 0
				}
			} else if mode1 == 0 && mode2 == 1 {
				if input[input[i+1]] == input[i+2] {
					input[input[i+3]] = 1
				} else {
					input[input[i+3]] = 0
				}
			} else if mode1 == 1 && mode2 == 0 {
				if input[i+1] == input[input[i+2]] {
					input[input[i+3]] = 1
				} else {
					input[input[i+3]] = 0
				}
			}

		} else if opcode == 99 {
			break

		}
	}
}

func readFile() {
	input, _ := ioutil.ReadFile(os.Args[1])
	arr := strings.Split(strings.TrimSpace(string(input)), ",")
	intcode = make([]int, len(arr))
	for i := range arr {
		intcode[i], _ = strconv.Atoi(arr[i])
	}
}
