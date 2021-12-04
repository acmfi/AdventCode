package main

import (
	"flag"
	"fmt"
	"os"
	"strconv"
	"strings"

	"github.com/pkg/profile"
)

var (
	inputFile = flag.String("f", "input.txt", "puzzle input file")
	profiling = flag.String("profile", "", "enable profiler, specify 'cpu' or 'mem'")
)

func panicOnErr(err error) {
	if err != nil {
		panic(err)
	}
}

type Row [5]int
type Board [5]Row

func checkBoard(board Board, drawn map[int]bool) (bool, int) {
	win := false
	unmarkedSum := 0

	for row := 0; row < len(board); row++ {
		rowsMarked := 0
		colsMarked := 0
		for col := 0; col < len(board[row]); col++ {
			// Check row (and sum unmarkeds)
			if drawn[board[row][col]] {
				rowsMarked++
			} else {
				unmarkedSum += board[row][col]
			}

			// Check columns: I know this is going to check them each row,
			// but is free to check them here instead in another double for
			// since the boards are squares. If they werent we would need to
			// go for anoter dobule for.
			if drawn[board[col][row]] {
				colsMarked++
			}
		}

		if rowsMarked == len(board[row]) || colsMarked == len(board) {
			win = true
		}
	}

	return win, unmarkedSum
}

func solve(balls []int, boards map[int]Board) (int, int) {
	scores := []int{}

	// Store drawn numbers in a bool map as it is convenient for
	// checking if a number is drawn when checking the boards.
	drawn := map[int]bool{}
	for _, n := range balls {
		drawn[n] = true

		for board := range boards {
			complete, unmarkedSum := checkBoard(boards[board], drawn)

			if complete {
				scores = append(scores, unmarkedSum*n)

				// Remove the board from the game to avoid
				// evaluating it when the next number is drawn.
				delete(boards, board)
			}
		}

		// If there are no more boards stop the game.
		if len(boards) == 0 {
			break
		}
	}

	return scores[0], scores[len(scores)-1]
}

func parseInput(input string) ([]int, map[int]Board) {
	lines := strings.Split(strings.TrimSpace(input), "\n")

	// Parse balls numbers (first line)
	balls := []int{}

	for _, nstr := range strings.Split(lines[0], ",") {
		n, err := strconv.Atoi(nstr)
		panicOnErr(err)

		balls = append(balls, n)
	}

	// Parse boards (skip two first lines: balls and next empty)
	boards := map[int]Board{}

	currentBoard := 0
	currentRow := 0

	board := Board{}
	for _, line := range lines[2:] {
		// Every new line a new board starts, so current row must be 0
		if strings.TrimSpace(line) == "" {
			if currentRow != 0 {
				panic("Malformed input file")
			}
			continue
		}

		// Process row
		var row Row
		for col, nstr := range strings.Fields(line) {
			n, err := strconv.Atoi(nstr)
			panicOnErr(err)

			row[col] = n
		}
		board[currentRow] = row

		currentRow++

		// Store board if we already read the 5 rows
		if currentRow == len(board) {
			boards[currentBoard] = board
			currentBoard++
			currentRow = 0
		}
	}

	return balls, boards
}

func main() {
	flag.Parse()

	// Profiler
	switch *profiling {
	case "cpu":
		defer profile.Start(profile.CPUProfile, profile.ProfilePath(".")).Stop()
	case "mem":
		defer profile.Start(profile.MemProfile, profile.ProfilePath(".")).Stop()
	}

	// Read file
	input, err := os.ReadFile(*inputFile)
	panicOnErr(err)

	// Parse input
	balls, boards := parseInput(string(input))

	// Solve
	star1, star2 := solve(balls, boards)

	fmt.Printf("Star 1: %d\n", star1)
	fmt.Printf("Star 2: %d\n", star2)
}
