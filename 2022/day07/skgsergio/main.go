package main

import (
	"flag"
	"fmt"
	"io/ioutil"
	"os"
	"sort"
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

type File struct {
	name string
	size int
}

func NewFile(name string, size int) *File {
	return &File{name: name, size: size}
}

func (f *File) Name() string {
	return f.name
}

func (f *File) Size() int {
	return f.size
}

type Dir struct {
	name      string
	parent    *Dir
	subDirs   []*Dir
	files     []*File
	sizeCache int
}

func NewDir(name string, parent *Dir) *Dir {
	return &Dir{name: name, parent: parent}
}

func (d *Dir) Name() string {
	return d.name
}

func (d *Dir) Parent() *Dir {
	return d.parent
}

func (d *Dir) SubDirs() []*Dir {
	return d.subDirs
}

func (d *Dir) Files() []*File {
	return d.files
}

func (d *Dir) Size() int {
	if d.sizeCache != 0 {
		return d.sizeCache
	}

	for _, s := range d.subDirs {
		d.sizeCache += s.Size()
	}

	for _, f := range d.files {
		d.sizeCache += f.Size()
	}

	return d.sizeCache
}

func (d *Dir) AddSubDir(s *Dir) {
	d.sizeCache = 0
	d.subDirs = append(d.subDirs, s)
}

func (d *Dir) AddFile(f *File) {
	d.sizeCache = 0
	d.files = append(d.files, f)
}

func (d *Dir) Cd(path string) (*Dir, error) {
	// We could implement here singe dot or relative paths (../x, ./x, x/y/z)
	// but is not required for the challenge.
	if path == ".." {
		return d.parent, nil
	}

	for _, s := range d.subDirs {
		if s.Name() == path {
			return s, nil
		}
	}

	return nil, fmt.Errorf("subdirectory '%s' not found in directory '%s'", path, d.name)
}

func ParseTermOutput(stdout string) *Dir {
	var err error = nil

	root := NewDir("/", nil)
	current := root
	listing := false

	for _, line := range strings.Split(strings.TrimSpace(stdout), "\n") {
		lineSplit := strings.Split(line, " ")

		if lineSplit[0] == "$" { // Shell
			listing = false

			if lineSplit[1] == "cd" {
				path := strings.Join(lineSplit[2:], " ")

				if path == "/" {
					current = root
				} else {
					current, err = current.Cd(path)
					panicOnErr(err)
				}
			} else if lineSplit[1] == "ls" {
				listing = true
			} else {
				panicOnErr(fmt.Errorf("unknown cmd '%s', line: %s", lineSplit[1], line))
			}

		} else { // Cmd output
			if listing {
				if lineSplit[0] == "dir" {
					current.AddSubDir(NewDir(strings.Join(lineSplit[1:], " "), current))
				} else {
					size, err := strconv.Atoi(lineSplit[0])
					panicOnErr(err)
					current.AddFile(NewFile(strings.Join(lineSplit[1:], " "), size))
				}
			} else {
				panicOnErr(fmt.Errorf("unknow type of output, line: %s", line))
			}
		}
	}

	return root
}

func FindDirsWithMaxSize(dir *Dir, size int) []*Dir {
	dirs := []*Dir{}

	for _, s := range dir.SubDirs() {
		dirs = append(dirs, FindDirsWithMaxSize(s, size)...)

		if s.Size() <= size {
			dirs = append(dirs, s)
		}
	}

	return dirs
}

func FindDirsWithMinSize(dir *Dir, size int) []*Dir {
	dirs := []*Dir{}

	for _, s := range dir.SubDirs() {
		dirs = append(dirs, FindDirsWithMinSize(s, size)...)

		if s.Size() >= size {
			dirs = append(dirs, s)
		}
	}

	return dirs
}

func solve(input *os.File) (int, int) {
	s1 := 0
	s2 := 0

	// Read whole file
	inputBytes, err := ioutil.ReadAll(input)
	panicOnErr(err)

	fs := ParseTermOutput(string(inputBytes))

	// Part 1
	maxSize := 100000

	for _, d := range FindDirsWithMaxSize(fs, maxSize) {
		s1 += d.Size()
	}

	// Part 2
	fsSize := 70000000
	reqFree := 30000000

	currentFree := fsSize - fs.Size()
	needToFree := reqFree - currentFree

	dirs := FindDirsWithMinSize(fs, needToFree)
	sort.Slice(dirs, func(i, j int) bool { return dirs[i].Size() < dirs[j].Size() })
	s2 = dirs[0].Size()

	return s1, s2
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

	// Open file
	file, err := os.Open(*inputFile)
	panicOnErr(err)
	defer file.Close()

	// Solve
	s1, s2 := solve(file)
	fmt.Printf("Star 1: %v\n", s1)
	fmt.Printf("Star 2: %v\n", s2)
}
