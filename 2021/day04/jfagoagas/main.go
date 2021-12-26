package main

import (
	"flag"
	"fmt"
	"os"
	"strconv"
	"strings"
)

var (
	inputPath = flag.String("f", "input", "Puzzle input path")
)

// Bingo card model
type Board struct {
	card [5]Row
	win  bool
}

// Every row contains 5 numbers
type Row [5]BingoNumber

// Every number
type BingoNumber struct {
	number int
	marked bool
}

func check(e error) {
	if e != nil {
		panic(e)
	}
}

func main() {
	flag.Parse()
	balls, boards := parseInput(*inputPath)
	star1, star2 := playBingo(balls, boards)
	fmt.Printf("Star 1: %d\n", star1)
	fmt.Printf("Star 2: %d\n", star2)
}

func (board *Board) markNumber(number int) {
	for row, r := range board.card {
		for column := range r {
			if !board.card[row][column].marked && board.card[row][column].number == number {
				board.card[row][column].marked = true
			}
		}
	}
}

func (board *Board) checkWinner() (bool, int) {
	sum := 0
	for row, r := range board.card {
		rowMark := 0
		columnMark := 0
		for column := range r {
			// Review rows
			if board.card[row][column].marked {
				rowMark++
			} else {
				sum += board.card[row][column].number
			}
			// Review columns
			if board.card[column][row].marked {
				columnMark++
			}
		}
		// We have a winner
		if rowMark == len(r) || columnMark == len(board.card) {
			board.win = true
		}
	}
	return board.win, sum
}

func playBingo(balls []int, boards []Board) (int, int) {
	// Count boards
	numWinners := 0
	// Let's play
	star1 := 0
	star2 := 0
	for _, number := range balls {
		// Mark numbers
		for n := range boards {
			board := &boards[n]
			if board.win {
				continue
			}
			board.markNumber(number)
			// Check winner board
			winner, score := board.checkWinner()
			if winner {
				numWinners++
				// First winner
				if star1 == 0 {
					// fmt.Println("First board winner!")
					// board.drawBoard()
					star1 = score * number
				}
				// Last winner
				if numWinners == len(boards) {
					// fmt.Println("Last board winner!")
					// board.drawBoard()
					star2 = score * number
				}
			}
		}
	}
	return star1, star2
}

func (board *Board) drawBoard() {
	for x := 0; x < 5; x++ {
		for y := 0; y < 5; y++ {
			print := ""
			if board.card[x][y].number < 10 {
				print += " "
			}
			if board.card[x][y].marked {
				// Yellow
				print += "\033[33m"
				print += strconv.Itoa(board.card[x][y].number)
				// Reset color
				print += "\033[0m"
			} else {
				print += strconv.Itoa(board.card[x][y].number)
			}
			fmt.Print(print + " ")
		}
		fmt.Printf("\n")
	}
	fmt.Printf("\n")
}

func parseInput(inputPath string) ([]int, []Board) {
	file, err := os.ReadFile(inputPath)
	check(err)

	lines := strings.Split(strings.TrimSpace(string(file)), "\n")

	// Store balls numbers
	numbers := []int{}
	for _, value := range strings.Split(lines[0], ",") {
		n, err := strconv.Atoi(string(value))
		check(err)
		numbers = append(numbers, n)
	}

	// Store boards
	currentRow := 0
	currentBoard := 0
	// New list of boards
	boards := []Board{}
	// New board
	board := Board{}
	for _, line := range lines[2:] {

		// Skip empty lines
		if line == "" {
			continue
		}
		column := 0
		// New row
		row := Row{}
		for _, number := range strings.Split(line, " ") {
			if number == "" {
				continue
			}
			if column > 4 {
				column = 0
				break
			}

			n, err := strconv.Atoi(string(number))
			check(err)
			element := BingoNumber{
				number: n,
				marked: false,
			}
			row[column] = element

			column++
		}

		board.card[currentRow] = row
		currentRow++

		if currentRow > 4 {
			board.win = false
			boards = append(boards, board)
			currentBoard++
			currentRow = 0
		}
	}
	return numbers, boards
}
