package main

import (
	"crypto/md5"
	"fmt"
	"io"
	"strconv"
	"strings"
)

func doMD5(s string) string {
	h := md5.New()
	io.WriteString(h, s)
	return fmt.Sprintf("%x", h.Sum(nil))
}

func contains(s []int, e int) bool {
	for _, a := range s {
		if a == e {
			return true
		}
	}
	return false
}

func getPos(s string) (int, error) {
	return strconv.Atoi(string(s[5]))
}

func validString(s string, used []int) bool {
	pref := strings.HasPrefix(s, "00000")
	if !pref {
		return false
	}

	i, err := getPos(s)
	return !(err != nil || i > 7 || contains(used, i))
}

func extractValue(s string) string {
	return string(s[6])
}

func main() {
	res := make([]string, 8)
	used := make([]int, 0)

	init := "abc"
	init = "ojvtpuvg"
	n := 0
	for len(used) < 8 {
		actual := fmt.Sprintf("%s%d", init, n)
		n++
		m := doMD5(actual)
		if validString(m, used) {
			fmt.Println(m)
			i, _ := getPos(m)
			c := extractValue(m)
			used = append(used, i)
			res[i] = c
		}
	}

	fmt.Println(strings.Join(res, ""))
}

// answer = 4543c154
// answer2 = 1050cbbd
