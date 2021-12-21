package main

import (
	"bufio"
	"flag"
	"fmt"
	"os"
	"strconv"
	"strings"
)

var (
	input = flag.String("f", "input", "Puzzle input file")
)

type Dice struct {
	value int
	rolls int
}

type Player struct {
	id       int
	position int
	score    int
}

func check(e error) {
	if e != nil {
		panic(e)
	}
}

func (d *Dice) Check() {
	if d.value > 100 {
		d.value = 1
	}
}

func (d *Dice) Roll() {
	d.value++
	d.rolls++
}

func (p *Player) checkPosition() int {
	position := 0
	if p.position%10 == 0 {
		position = 10
	} else {
		position = p.position % 10
	}
	return position
}

func (p *Player) play(dice *Dice) int {
	//fmt.Printf("Player %d rolls ", p.id)
	// Roll dice three times
	for i := 0; i < 3; i++ {
		dice.Check()
		p.position += dice.value
		//fmt.Printf("%d ", dice.value)
		dice.Roll()
	}
	// Check circular position
	p.position = p.checkPosition()
	//fmt.Printf("and moves to space %d ", p.position)
	return p.position
}

func min(player1, player2 int) int {
	min := 0
	if player1 < player2 {
		min = player1
	} else {
		min = player2
	}
	return min
}

func game(players []Player) int {
	dice := &Dice{
		value: 1,
		rolls: 0,
	}
	score := 0
	for score < 1000 {
		// Play
		for i, p := range players {
			// Save the new position
			players[i].position = p.play(dice)

			// Update the score
			players[i].score += players[i].position

			//fmt.Println("for a total score of", players[i].score)
			// Global score
			score = players[i].score
			if score == 1000 {
				break
			}
		}
	}
	return min(players[0].score, players[1].score) * dice.rolls
}

func main() {
	flag.Parse()
	players := parseInput(*input)
	fmt.Printf("Star 1: %d\n", game(players))
}

func parseInput(inputPath string) []Player {
	file, err := os.Open(inputPath)
	check(err)
	defer func() {
		err := file.Close()
		check(err)
	}()

	players := []Player{}
	scanner := bufio.NewScanner(file)
	playerID := 1
	for scanner.Scan() {
		line := strings.Split((scanner.Text()), ":")
		position, err := strconv.Atoi(strings.TrimSpace(line[1]))
		check(err)
		player := Player{
			id:       playerID,
			position: position,
			score:    0,
		}
		players = append(players, player)
		playerID++
	}
	return players
}
