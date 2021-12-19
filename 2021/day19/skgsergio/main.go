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

type Point3D struct {
	x int
	y int
	z int
}

func (p *Point3D) Add(other Point3D) Point3D {
	return Point3D{p.x + other.x, p.y + other.y, p.z + other.z}
}

func (p *Point3D) Sub(other Point3D) Point3D {
	return Point3D{p.x - other.x, p.y - other.y, p.z - other.z}
}

func (p *Point3D) Manhattan(other Point3D) int {
	d := p.Sub(other)
	return abs(d.x) + abs(d.y) + abs(d.z)
}

func (p Point3D) Up(idx int) Point3D {
	return []Point3D{
		p,
		{p.x, p.y, p.z},
		{p.x, -p.y, -p.z},
		{p.x, -p.z, p.y},
		{-p.y, -p.z, p.x},
		{-p.x, -p.z, -p.y},
		{p.y, -p.z, -p.x},
	}[idx]
}

const P3D_UP_LEN = 7

func (p Point3D) Rot(idx int) Point3D {
	return []Point3D{
		p,
		{-p.y, p.x, p.z},
		{-p.x, -p.y, p.z},
		{p.y, -p.x, p.z},
	}[idx]
}

const P3D_ROT_LEN = 4

type Scanner struct {
	beacons []Point3D
}

func (s Scanner) Rotations() []Scanner {
	rotScanners := []Scanner{}

	for u := 0; u < P3D_UP_LEN; u++ {
		for r := 0; r < P3D_ROT_LEN; r++ {
			scanner := Scanner{}

			for _, b := range s.beacons {
				scanner.beacons = append(scanner.beacons, b.Up(u).Rot(r))
			}

			rotScanners = append(rotScanners, scanner)
		}
	}

	return rotScanners
}

func abs(n int) int {
	if n < 0 {
		return -n
	}
	return n
}

func allIs(boolMap map[int]bool, value bool) bool {
	for _, v := range boolMap {
		if v != value {
			return false
		}
	}

	return true
}

func solve(scanners map[int]*Scanner) (int, int) {
	finalBeacons := map[Point3D]bool{}
	for _, b := range scanners[0].beacons {
		finalBeacons[b] = true
	}

	lockedScanners := map[int]bool{}
	for i := range scanners {
		lockedScanners[i] = i == 0
	}

	finalScanners := []Point3D{}
	for i := 0; !allIs(lockedScanners, true); i++ {
		// Reset scanner id
		if _, ok := lockedScanners[i]; !ok {
			i = 0
		}

		if lockedScanners[i] {
			continue
		}

	scannerRotationsLoop:
		// For this scanner iterate all possible rotations
		for _, scannerRotation := range scanners[i].Rotations() {
			// Check each final beacon with all beacons in the rotated scanner
			for finalBeacon := range finalBeacons {
				for _, initial := range scannerRotation.beacons {
					diff := initial.Sub(finalBeacon)

					matches := 0
					remaining := len(scannerRotation.beacons)

					for _, candidate := range scannerRotation.beacons {
						// Check if the candidate - diff is present
						if finalBeacons[candidate.Sub(diff)] {
							matches++
						}
						remaining--

						// If we hace at least 12 matches store scanner position and final beacon
						if matches >= 12 {
							for _, b := range scannerRotation.beacons {
								finalScanners = append(finalScanners, diff)
								finalBeacons[b.Sub(diff)] = true
							}
							lockedScanners[i] = true

							break scannerRotationsLoop
						}

						// Small optimization: do not continue checking if there are not enough remaining points
						if (matches + remaining) < 12 {
							break
						}
					}
				}
			}
		}
	}

	max := 0
	for i, sc1 := range finalScanners {
		for _, sc2 := range finalScanners[i:] {
			dist := sc1.Manhattan(sc2)
			if dist > max {
				max = dist
			}
		}
	}

	return len(finalBeacons), max
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
	scanners := map[int]*Scanner{}
	scanner := 0
	for _, line := range strings.Split(strings.TrimSpace(string(input)), "\n") {
		line = strings.TrimSpace(line)
		if line == "" {
			continue
		} else if strings.HasPrefix(line, "---") {
			s := strings.Split(line, " ")

			scanner, err = strconv.Atoi(s[2])
			panicOnErr(err)

			scanners[scanner] = &Scanner{}
		} else {
			s := strings.Split(line, ",")

			x, err := strconv.Atoi(s[0])
			panicOnErr(err)
			y, err := strconv.Atoi(s[1])
			panicOnErr(err)
			z, err := strconv.Atoi(s[2])
			panicOnErr(err)

			scanners[scanner].beacons = append(scanners[scanner].beacons, Point3D{x, y, z})
		}
	}

	// Solve
	star1, star2 := solve(scanners)
	fmt.Printf("Star 1: %d\n", star1)
	fmt.Printf("Star 2: %d\n", star2)
}
