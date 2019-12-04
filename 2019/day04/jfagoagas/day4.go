package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
)

func main() {
	var minVal, maxVal int = readInput()
	var num []string = make([]string, 0)
	num = seq(minVal, maxVal)
	var count int = findPwdCount(num)
	fmt.Printf("Part 1 -- Count: %d\n", count)
}

func findPwdCount(num []string) (count int) {
	count = 0
	for i := range num {
		if num[i][0:1] <= num[i][1:2] {
			if num[i][1:2] <= num[i][2:3] {
				if num[i][2:3] <= num[i][3:4] {
					if num[i][3:4] <= num[i][4:5] {
						if num[i][4:5] <= num[i][5:6] {
							if num[i][0:1] < num[i][1:2] && num[i][1:2] < num[i][2:3] && num[i][2:3] < num[i][3:4] && num[i][3:4] < num[i][4:5] && num[i][4:5] < num[i][5:6] {
							} else {
                                //fmt.Printf("%s\n", num[i])
                                count++
						    }
					    }
				    }
			    }
		    }
	    }
    }
	return
}

func seq(min, max int) []string {
	a := make([]string, max-min+1)
	for i := range a {
		a[i] = strconv.Itoa(min + i)
	}
	return a
}

func readInput() (minVal, maxVal int) {
	input := os.Args[1]
	s := strings.Split(input, "-")
	minVal, _ = strconv.Atoi(s[0])
	maxVal, _ = strconv.Atoi(s[1])
	return
}
