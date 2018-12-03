import java.io.File

fun main(args: Array<String>) {
    println("part1: " + part1("2018/day01/sami286/input"))
    println("part2: " + part2("2018/day01/sami286/input"))
}

fun part1(input: String): Int = File(input)
    .readLines()
    .map { it.toInt() }
    .sum()

fun part2(input: String): Int {
    var count = 0
    val seen = mutableSetOf(count)
    val numbers = File(input).readLines().map { it.toInt() }
    var i = 0
    while (true) {
        count += numbers[i]
        if (seen.contains(count)) return count
        seen.add(count)

        i++
        if (i == numbers.size)
            i = 0
    }
}