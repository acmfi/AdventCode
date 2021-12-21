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

func maxInt(x, y int) int {
	if x > y {
		return x
	}
	return y
}

type Player struct {
	position int
	score    int
}

func (p *Player) Turn(d *DeterministicDice) {
	roll := 0
	for i := 0; i < 3; i++ {
		roll += d.Roll()
	}
	p.position = ((p.position + roll - 1) % 10) + 1
	p.score += p.position
}

func (p *Player) Win() bool {
	return p.score >= 1000
}

func (p *Player) Score() int {
	return p.score
}

type DeterministicDice struct {
	value int
	rolls int
}

func (d *DeterministicDice) Roll() int {
	d.value++
	d.rolls++

	if d.value > 100 {
		d.value -= 100
	}

	return d.value
}

func (d *DeterministicDice) TimesRolled() int {
	return d.rolls
}

func star1(pos1, pos2 int) int {
	p1 := Player{pos1, 0}
	p2 := Player{pos2, 0}
	dice := &DeterministicDice{0, 0}

	res := 0

	for {
		p1.Turn(dice)
		if p1.Win() {
			res = p2.Score() * dice.TimesRolled()
			break
		}

		p2.Turn(dice)
		if p2.Win() {
			res = p1.Score() * dice.TimesRolled()
			break
		}
	}

	return res
}

type QPlayer struct {
	position int
	score    int
}

func (p *QPlayer) Turn(roll int) {
	p.position = ((p.position + roll - 1) % 10) + 1
	p.score = p.score + p.position
}

func (p *QPlayer) Win() bool {
	return p.score >= 21
}

type QRound struct {
	p1   QPlayer
	p2   QPlayer
	roll int
	turn int
}

type Result map[int]int
type QRoundResult map[QRound]Result

func play(p1, p2 QPlayer, roll, round int, rolls *[]int, qroundCache *QRoundResult) Result {
	qround := QRound{p1, p2, roll, round % 2}

	// Check if we already computed this round
	if res, ok := (*qroundCache)[qround]; ok {
		return res
	}

	// Compute player turn and check if won
	if qround.turn == 0 {
		p1.Turn(roll)

		if p1.Win() {
			(*qroundCache)[qround] = Result{1: 1}
			return (*qroundCache)[qround]
		}
	} else {
		p2.Turn(roll)

		if p2.Win() {
			(*qroundCache)[qround] = Result{2: 1}
			return (*qroundCache)[qround]
		}
	}

	// If none of the players won recurse the next round
	result := Result{}
	for _, nextRoll := range *rolls {
		roundResult := play(p1, p2, nextRoll, round+1, rolls, qroundCache)
		result[1] += roundResult[1]
		result[2] += roundResult[2]
	}
	(*qroundCache)[qround] = result

	return (*qroundCache)[qround]
}

func star2(pos1, pos2 int) int {
	playerUniversesWon := Result{}

	// Make all combintions of 3 rolls of 3-sided dice
	rolls := []int{}
	diceSides := []int{1, 2, 3}
	for i := range diceSides {
		for j := range diceSides {
			for k := range diceSides {
				rolls = append(rolls, diceSides[i]+diceSides[j]+diceSides[k])
			}
		}
	}

	// QRound Cache for speeding up the process when the a same status is reached
	qroundCache := QRoundResult{}

	// Start playing for each value recursively
	for _, roll := range rolls {
		roundResult := play(QPlayer{pos1, 0}, QPlayer{pos2, 0}, roll, 0, &rolls, &qroundCache)
		playerUniversesWon[1] += roundResult[1]
		playerUniversesWon[2] += roundResult[2]
	}

	return maxInt(playerUniversesWon[1], playerUniversesWon[2])
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
	player1 := 0
	player2 := 0

	for _, line := range strings.Split(strings.TrimSpace(string(input)), "\n") {
		s := strings.Fields(line)
		p, err := strconv.Atoi(s[4])
		panicOnErr(err)

		switch s[1] {
		case "1":
			player1 = p
		case "2":
			player2 = p
		default:
			panic("Unknown player.")
		}
	}

	// Solve
	fmt.Printf("Star 1: %d\n", star1(player1, player2))
	fmt.Printf("Star 2: %d\n", star2(player1, player2))
}
