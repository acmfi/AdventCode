package main

import (
	"fmt"
	"io/ioutil"
	"os"
	"strconv"
	"strings"
)

type numberGame struct {
	times     int
	lastRound []int
}

func main() {
	file, _ := ioutil.ReadFile(os.Args[1])
	input := strings.Split((string(file)), ",")
	gameMemory := map[int]numberGame{}

	i := 0
	lastNumber := 0
	roundNumber, _ := strconv.Atoi(os.Args[2])
	for round := 0; round < roundNumber; round++ {
		roundGame := numberGame{}
		roundGame.lastRound = append(roundGame.lastRound, round+1)

		number, _ := strconv.Atoi(input[i])
		if i == len(input)-1 {
			i = 0
		} else {
			i++
		}

		// if number do not exist --> starting number
		if _, ok := gameMemory[number]; !ok {
			roundGame.times = 1
			gameMemory[number] = roundGame
			lastNumber = number
			fmt.Printf("Round: %d -- Last Number: %d\n", round+1, lastNumber)
			continue
		}
		// if number has been spoken before only once
		if gameMemory[lastNumber].times == 1 {
			lastNumber = 0

			roundGame.times = gameMemory[lastNumber].times + 1
			roundGame.lastRound = append(gameMemory[lastNumber].lastRound, round+1)
			gameMemory[lastNumber] = roundGame
			fmt.Printf("Round: %d -- Last Number: %d\n", round+1, lastNumber)
		} else {
			//if number has been spoken before only once
			rounds := gameMemory[lastNumber].lastRound
			lastNumber = rounds[len(rounds)-1] - rounds[len(rounds)-2]

			roundGame.times = gameMemory[lastNumber].times + 1
			roundGame.lastRound = append(gameMemory[lastNumber].lastRound, round+1)
			gameMemory[lastNumber] = roundGame

			fmt.Printf("Round: %d -- Last Number: %d\n", round+1, lastNumber)
		}
		//fmt.Println("Game Memory:", gameMemory)
	}
}
