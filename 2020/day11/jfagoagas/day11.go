package main

import (
	"fmt"
	"io/ioutil"
	"os"
	"strings"
)

type point struct {
	X int
	Y int
}

func main() {
	file, _ := ioutil.ReadFile(os.Args[1])
	input := strings.Split((string(file)), "\n")

	var matrix1 [][]string
	var row1 []string

	emptySeat := "L"
	occupiedSeat := "#"

	for i := 0; i < len(input); i++ {
		row1 = nil
		for j := 0; j < len(input[i]); j++ {
			row1 = append(row1, string(input[i][j]))
		}
		matrix1 = append(matrix1, row1)
	}

	rowLength := len(matrix1)
	columnLength := len(matrix1[0])
	seatsOccupiedPreviously := 0
	for {
		var matrix2 [][]string
		//matrix2 := make([][]string, rowLength)
		for i := 0; i < rowLength; i++ {
			//row2 = nil
			var row2 []string
			for j := 0; j < columnLength; j++ {
				//row := matrix1[i]
				seat := matrix1[i][j]
				//fmt.Println("ROW BEFORE", row)
				//fmt.Printf("SEAT: %s -- Position [%d,%d]\n", seat, i, j)

				var s1 point
				s1.X = i
				s1.Y = j

				countOccupiedAdjacent := 0
				for ii := i - 1; ii <= i+1; ii++ {
					for jj := j - 1; jj <= j+1; jj++ {
						var s2 point
						s2.X, s2.Y = ii, jj
						//s2.Y = jj
						if ii >= 0 && ii < rowLength && jj >= 0 && jj < columnLength && s1 != s2 {
							//fmt.Printf("Adjacents -- [%d,%d] -- %s\n", ii, jj, matrix1[ii][jj])
							if matrix1[ii][jj] == occupiedSeat {
								countOccupiedAdjacent++
							}
						}
					}
				}
				//fmt.Println("Count Occupied Adjacent:", countOccupiedAdjacent)
				if seat == emptySeat && countOccupiedAdjacent == 0 {
					row2 = append(row2, "#")
				} else if seat == occupiedSeat && countOccupiedAdjacent >= 4 {
					row2 = append(row2, "L")
				} else {
					row2 = append(row2, matrix1[i][j])
				}
				//fmt.Println("ROW AFTER", line)
				//fmt.Println("")
				//fmt.Println(row)
			}
			matrix2 = append(matrix2, row2)
		}

		seatsOccupied := 0
		for i := range matrix2 {
			for j := range matrix2[i] {
				if matrix2[i][j] == "#" {
					seatsOccupied++
				}
			}

		}

		if seatsOccupied == seatsOccupiedPreviously {
			//fmt.Println(seatsOccupied)
			break
		}
		/*
			fmt.Println("OLD")
			for i := range matrix1 {
				fmt.Println(matrix1[i])
			}
			fmt.Println("NEW")
			for i := range matrix2 {
				fmt.Println(matrix2[i])
			}
		*/
		matrix1 = make([][]string, rowLength)
		copy(matrix1, matrix2)
		seatsOccupiedPreviously = seatsOccupied
	}
	fmt.Printf("Day 11\nPart 1: %d\n", seatsOccupiedPreviously)
}
