package main

import (
	"fmt"
	"io/ioutil"
	"os"
	"regexp"
	"strconv"
	"strings"
)

/*
byr (Birth Year) - four digits; at least 1920 and at most 2002.
iyr (Issue Year) - four digits; at least 2010 and at most 2020.
eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
hgt (Height) - a number followed by either cm or in:
If cm, the number must be at least 150 and at most 193.
If in, the number must be at least 59 and at most 76.
hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
pid (Passport ID) - a nine-digit number, including leading zeroes.
cid (Country ID) - ignored, missing or not.
*/

func main() {
	input, _ := ioutil.ReadFile(os.Args[1])
	var passportFields = []string{"byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"}
	valid := 0
out:
	for _, passportList := range strings.Split(strings.TrimSpace(string(input)), "\n\n") {
		// All fields in one line
		initList := strings.Replace(passportList, "\n", " ", -1)
		// First, check if each passport contains mandatory fields
		for _, fields := range passportFields {
			if ok := strings.Contains(passportList, fields); !ok {
				continue out
			}
		}
		// Second, check if each field contains values in range
		passportValues := strings.Split(initList, " ")
		for i := range passportValues {
			fields := strings.Split(string(passportValues[i]), ":")
			field := fields[0]
			value := fields[1]
			if field == "byr" {
				year, _ := strconv.Atoi(value)
				if year < 1920 || year > 2002 {
					continue out
				}
			}
			if field == "iyr" {
				year, _ := strconv.Atoi(value)
				if year < 2010 || year > 2020 {
					continue out
				}
			}
			if field == "eyr" {
				year, _ := strconv.Atoi(value)
				if year < 2020 || year > 2030 {
					continue out
				}
			}
			if field == "hgt" {
				number, _ := strconv.Atoi(value[0 : len(value)-2])
				unit := string(value[len(value)-2]) + string(value[len(value)-1])
				if unit == "cm" {
					if number < 150 || number > 193 {
						continue out
					}
				} else if unit == "in" {
					if number < 59 || number > 76 {
						continue out
					}
				} else {
					continue out
				}
			}
			if field == "hcl" {
				regexp, _ := regexp.Compile("#[0-9a-z]{6}")
				match := regexp.Match([]byte(value))
				if !match {
					continue out
				}
			}
			if field == "ecl" {
				regexp, _ := regexp.Compile("amb|blu|brn|gry|grn|hzl|oth")
				match := regexp.Match([]byte(value))
				if !match {
					continue out
				}
			}
			// pid (Passport ID) - a nine-digit number, including leading zeroes.
			if field == "pid" {
				regexp, _ := regexp.Compile("[0-9]{9}")
				match := regexp.Match([]byte(value))
				if !match {
					continue out
				}
			}
		}
		valid++
	}
	fmt.Printf("Day 4 -- Part 2: %d\n", valid)
}
