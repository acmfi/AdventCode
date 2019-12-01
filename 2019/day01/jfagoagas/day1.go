package main
import (
    "fmt"
    "bufio"
    "os"
    "strconv"
)

func main() {
    input := os.Args[1]
    file, err := os.Open(input)
    if err != nil {
        os.Exit(1)
    }
    defer file.Close()

    scanner := bufio.NewScanner(file)
    var total int = 0
    var fuel int = 0
    for scanner.Scan() {
        mass, _ := strconv.Atoi(scanner.Text())
        // Part 1
        fuel = calcFuel(mass)
        // Part 2
        for fuel > 0 {
            total += fuel
            fuel = calcFuel(fuel)
        }
    }
    fmt.Printf("%d\n", total)
}

func calcFuel(mass int) (fuel int) {
    fuel = (mass / 3) - 2
    return
}
