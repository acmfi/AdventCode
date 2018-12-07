import java.io.File
import kotlin.math.abs
import kotlin.math.max

fun main() {
    val (p1, p2) = part1and2("2018/day06/sami286/input")
    println("part1: $p1")
    println("part2: $p2")
}

fun part1and2(input: String): Pair<Int, Int> {
    var maxX = 0
    var maxY = 0
    val data = File(input).readLines().map {
        val x = it.substring(0, it.indexOf(",")).toInt()
        val y = it.substring(it.indexOf(",") + 2).toInt()
        maxX = max(maxX, x)
        maxY = max(maxY, y)
        Pair(x, y)
    }

    val zoneCount = IntArray(data.size)
    val zoneExcluded = mutableListOf<Int>()
    var zoneSafe = 0
    for (i in 0..maxX) {
        for (j in 0..maxY) {
            val distances = data.map { abs(it.first - i) + abs(it.second - j) }
            if (distances.sum() < 10000) zoneSafe++
            val minDist = distances.min()
            if (distances.filter { it == minDist }.size == 1) {
                val id = distances.indexOf(minDist)
                zoneCount[id]++
                if (i == 0 || j == 0 || i == maxX || j == maxY)
                    zoneExcluded += id
            }
        }
    }
    zoneExcluded.forEach { zoneCount[it] = -1 }
    return Pair(zoneCount.max()!!, zoneSafe)
}