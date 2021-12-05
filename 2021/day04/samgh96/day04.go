package main

import (
        "fmt"
        "log"
        "os"
        "strconv"
        "strings"
)

type bingoNum struct {
        number int
        marked bool
}

type bingoCard struct {
        numbers [5][5]bingoNum
        won bool
}

func checkLine(line [5]bingoNum) bool {
        result := true
        for i := 0; result && i < len(line); i++ {
                result = result && line[i].marked
        }

        return result
}

func (board *bingoCard) checkBoard() bool {
        result := false
        for i := 0; !result && i < len(board.numbers); i++ {
                column := [5]bingoNum {
                        board.numbers[0][i],
                                board.numbers[1][i],
                                board.numbers[2][i],
                                board.numbers[3][i],
                                board.numbers[4][i],
                        }
                result = result || checkLine(board.numbers[i]) || checkLine(column)
        }

        board.won = result

        return result
}

func markNum(draw int, board *bingoCard) {
        for i, row := range &board.numbers {
                for j, num := range &row {
                        if num.number == draw {
                                board.numbers[i][j].marked = true
                        }
                }
        }
}

func (board bingoCard) score(lastDraw int) int {
        unmarkedSum := 0
        for _, row := range board.numbers {
                for _, num := range row {
                        if !num.marked {
                                unmarkedSum += num.number
                        }
                }
        }

        return unmarkedSum * lastDraw
}

func star2(draws []int, boards []bingoCard) {
        winnerBoards := []bingoCard{}
        winnerDraws := []int{}

        lastWin := false

        for j := 0; !lastWin && j < len(draws); j++ {
                winnerDraws = append(winnerDraws, draws[j])

                for i := 0; i < len(boards); i++ {
                        markNum(draws[j], &boards[i])
                        if !boards[i].won && boards[i].checkBoard() {
                                winnerBoards = append(winnerBoards, boards[i])
                        }
                }
		
		lastWin = len(winnerBoards) == len(boards)
        }

        fmt.Printf("star 2: %d\n", winnerBoards[len(winnerBoards) - 1].score(winnerDraws[len(winnerDraws) - 1]))
}

func star1(draws []int, boards []bingoCard) {
        winnerBoard := bingoCard{}
        winnerDraw := -1

        for _, draw := range draws {
                for i := 0; winnerDraw == -1 && i < len(boards); i++ {
                        markNum(draw, &boards[i])
                        if boards[i].checkBoard() {
                                winnerBoard = boards[i]
                                winnerDraw = draw
                        }
                }
        }

        fmt.Printf("star 1: %d\n", winnerBoard.score(winnerDraw))
}

func main() {
        rawInput, err := os.ReadFile(os.Args[1])
        if err != nil {
                log.Fatal(err)
                return
        }

        sectionedInput := strings.Split(string(rawInput), "\n\n")

        drawableNumbers := []int{}
        for _, num := range strings.Split(sectionedInput[0], ",") {
                convertedNum, err := strconv.Atoi(num)
                if err != nil {
                        log.Fatal(err)
                        return
                }

                drawableNumbers = append(drawableNumbers, convertedNum)
        }

        boards := []bingoCard{}
        for _, strBoard := range sectionedInput[1:] {
                dividedBoard := strings.Fields(strBoard)

                board := bingoCard{
                        numbers: [5][5]bingoNum{},
                        won: false,
                }

                rowIndex := -1
                for i, strNum := range dividedBoard {
                        num, err := strconv.Atoi(strNum)
                        if err != nil {
                                log.Fatal(err)
                                return
                        }

                        colIndex := i % 5
                        if colIndex == 0 {
                                rowIndex++
                        }

                        board.numbers[rowIndex][colIndex] = bingoNum{num, false}
                }

                boards = append(boards, board)
        }

        // star1(drawableNumbers, boards)
        star2(drawableNumbers, boards)
}
