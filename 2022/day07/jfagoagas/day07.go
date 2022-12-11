package main

import (
	"bufio"
	"flag"
	"fmt"
	"os"
	"strconv"
	"strings"
	"unicode"
)

var (
	inputFile = flag.String("f", "input.txt", "Puzzle input file")
)

func exitOnError(err error) {
	if err != nil {
		fmt.Println(err)
		os.Exit(1)
	}
}

func readLines(path string) []string {
	file, err := os.Open(path)
	exitOnError(err)
	defer func() {
		err := file.Close()
		exitOnError(err)
	}()

	scanner := bufio.NewScanner(file)

	var lines []string
	for scanner.Scan() {
		lines = append(lines, scanner.Text())
	}
	return lines
}

type directory struct {
	name        string
	parent      *directory
	directories []*directory
	files       []*file
	size        int
}

type file struct {
	name string
	size int
}

func listDirectories(parentDirectory *directory, sizeUnder100000 *[]int, allSizes *[]int) (*[]int, *[]int) {
	directorySize := 0
	// fmt.Printf("Directory %s\n", parentDirectory.name)
	for _, dir := range parentDirectory.directories {
		listDirectories(dir, sizeUnder100000, allSizes)
	}
	// If there is no more subdirectories
	subDirectorySize := listFiles(parentDirectory)
	parentDirectory.size += subDirectorySize
	// fmt.Printf("Sub directory: %s - Size: %d\n", parentDirectory.name, parentDirectory.size)

	// PART 1
	if parentDirectory.size <= 100000 {
		// fmt.Printf("Size under 100000: %d\n", parentDirectory.size)
		*sizeUnder100000 = append(*sizeUnder100000, parentDirectory.size)

	}
	// PART 2
	*allSizes = append(*allSizes, parentDirectory.size)

	directorySize += subDirectorySize
	// If the directory is not the root folder, add the size to the parent folder
	if parentDirectory.parent != nil {
		// fmt.Printf("PRE - Parent Directory: %s - Size: %d\n", parentDirectory.parent.name, parentDirectory.parent.size)
		// parentDirectory.parent.size += directorySize
		sumParentDirectorySize(parentDirectory, directorySize)
		// fmt.Printf("POST - Parent Directory: %s - Size: %d\n\n", parentDirectory.parent.name, parentDirectory.parent.size)
	}
	return sizeUnder100000, allSizes
}

// sumParentDirectorySize adds the actual directory size to the parent directory
func sumParentDirectorySize(parentDirectory *directory, size int) {
	// If the parent is not root we need to keep iterating recursively
	if parentDirectory.parent != nil {
		// fmt.Printf("Adding to the top parent directory '%s' the size '%d'\n", parentDirectory.parent.name, size)
		sumParentDirectorySize(parentDirectory.parent, size)
	}
	// If there is no parent this is the top of the tree
	parentDirectory.size += size
}

// listFiles returns the directory size
func listFiles(parentDirectory *directory) int {
	directorySize := 0
	for _, file := range parentDirectory.files {
		// fmt.Printf("- File: %s\n", file.name)
		directorySize += file.size
	}
	return directorySize
}

func execCommands(commands []string, root *directory) {
	previousDirectory := root
	actualDirectory := root

	for _, line := range commands {
		// Check if the line contains a command
		if line[0] == '$' {
			// fmt.Printf("Command: %s\n", line)
			// Create the root folder
			if strings.Contains(line, "cd") {
				// $ cd ..
				// destinationDir = ".."
				destinationDirectory := strings.Split(line, " ")[2]
				// root folder
				if destinationDirectory == "/" {
					actualDirectory = root
					previousDirectory = root
					continue
					// parent folder
				} else if destinationDirectory == ".." {
					actualDirectory = actualDirectory.parent
					continue
				} else {
					// fmt.Printf("Destination directory %s\n", destinationDirectory)
					if previousDirectory.name != "root" {
						previousDirectory = actualDirectory
					}
					destinationDirectoryExists := false
					for _, dir := range actualDirectory.directories {
						if dir.name == destinationDirectory {
							actualDirectory = dir
							destinationDirectoryExists = true
							break
						}
					}
					if !destinationDirectoryExists {
						fmt.Println("Destination directory does not exists.")
						break
					}
				}
			}

			// Check if line runs `ls`
			// line == "$ ls"
			if strings.Contains(line, "ls") {
				// Nothing to do here
			}
		}

		// Check if line starts by "dir"
		// dir e
		// Create a new dir
		if strings.Contains(line, "dir") {
			// fmt.Printf("Actual directory name: %s\n", actualDirectory.name)
			directoryName := strings.Split(line, " ")[1]
			// fmt.Printf("Directory name: %s\n", directoryName)
			newDirectory := &directory{
				name:        directoryName,
				parent:      actualDirectory,
				directories: []*directory{},
				files:       []*file{},
			}
			actualDirectory.directories = append(actualDirectory.directories, newDirectory)
		}
		// If the line starts by a number
		// 14848514 b.txt
		// Create a new file
		if unicode.IsDigit(rune(line[0])) {
			elements := strings.Split(line, " ")
			fileSize, err := strconv.Atoi(elements[0])
			exitOnError(err)
			fileName := elements[1]
			// fmt.Printf("File name: %s\n", fileName)
			newFile := &file{
				name: fileName,
				size: fileSize,
			}
			actualDirectory.files = append(actualDirectory.files, newFile)
		}
		fmt.Println()
	}
}

func sumDirectorySize(root *directory) (int, []int) {
	sizeUnder100000 := []int{}
	allSizes := []int{}
	listDirectories(root, &sizeUnder100000, &allSizes)

	total := 0
	for _, dirSize := range sizeUnder100000 {
		total += dirSize
	}
	return total, allSizes
}

func main() {
	// Parse Flags
	flag.Parse()
	// Read Input file
	input := readLines(*inputFile)
	// Create the root folder
	root := &directory{
		name:        "root",
		parent:      nil,
		directories: []*directory{},
		files:       []*file{},
	}
	// Run commands and build filesystem
	execCommands(input, root)
	part1, _ := sumDirectorySize(root)

	fmt.Printf("Part 1 - Result: %d\n", part1)
}
