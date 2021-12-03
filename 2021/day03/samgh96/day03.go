package main

import (
        "fmt"
        "log"
        "os"
        "strconv"
        "strings"
)


func rating(ratingType string, position int, candidates []string) uint64 {
        if len(candidates) == 1 {
                num, err := strconv.ParseUint(candidates[0], 2, 64)
                if err != nil {
                        log.Fatal(err)
                }
                return num
        }

        ones := []string{}
        zeroes := []string{}

        for _, candidate := range candidates {
                if candidate[position] == '1' {
                        ones = append(ones, candidate)
                } else {
                        zeroes = append(zeroes, candidate)
                }
        }
	
        if ratingType == "oxygen" {
                if len(ones) >= len(zeroes) {
                        return rating(ratingType, position + 1, ones)
                } else {
                        return rating(ratingType, position + 1, zeroes)
                }
	} else {
		if len(ones) < len(zeroes) {
                        return rating(ratingType, position + 1, ones)
                } else {
                        return rating(ratingType, position + 1, zeroes)
                }
	}

}

func star2(bin []string) {
	CO2 := rating("CO2", 0, bin)
	O2 := rating("oxygen", 0, bin)

	fmt.Printf("star 2: %d\n", CO2 * O2)
}

func star1(bin []string) {
        gammaRate := ""
        epsilonRate := ""
        for _, s := range bin {
                if strings.Count(s, "1") >= len(s) / 2 {
                        gammaRate += "1"
                        epsilonRate += "0"
                } else {
                        gammaRate += "0"
                        epsilonRate += "1"
                }
        }

        gammaBin, err1 := strconv.ParseUint(gammaRate, 2, 64)
        epsilonBin, err2 := strconv.ParseUint(epsilonRate, 2, 64)
        if err1 == nil && err2 == nil {
                fmt.Printf("star 1: %d\n", gammaBin * epsilonBin)
        }
}

func main() {
        rawInput, err := os.ReadFile(os.Args[1])
        if err != nil {
                log.Fatal(err)
                return
        }

        parsedHorizontalBin := strings.Split(string(rawInput), "\n")
        parsedVerticalBin := []string{}
        for i := range parsedHorizontalBin[0] {
                verticalBin := ""

                for j := 0; j < len(parsedHorizontalBin); j++ {
                        verticalBin += string(parsedHorizontalBin[j][i])
                }

                parsedVerticalBin = append(parsedVerticalBin, verticalBin)
        }

        star1(parsedVerticalBin)
        star2(parsedHorizontalBin)
}
