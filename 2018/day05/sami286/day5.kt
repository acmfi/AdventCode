import java.io.File
import kotlin.math.min

fun main() {
    val polymer = part1("2018/day05/sami286/input")
    println("part1: " + polymer.length)
    println("part2: " + part2(polymer))
}

fun part1(input: String): String = reduce(File(input).readLines()[0])

fun part2(polymerLong: String): Int {
    var sizeMin = Int.MAX_VALUE
    for (type in 'a'..'z') {
        val polymer = polymerLong.filter { !type.equals(it, true) }
        sizeMin = min(sizeMin, reduce(polymer).length)
    }
    return sizeMin
}

fun reduce(polymerLong: String): String {
    var polymer = polymerLong
    while (true) {
        val react = mutableListOf<Int>()
        var i = 0
        while (i < polymer.length - 1) {
            if (polymer[i].equals(polymer[i + 1], true) && polymer[i] != polymer[i + 1]) {
                react.add(i)
                i++
            }
            i++
        }
        if (react.isEmpty()) return polymer
        react.reversed().forEach {
            polymer = polymer.removeRange(it, it + 2)
        }
    }
}