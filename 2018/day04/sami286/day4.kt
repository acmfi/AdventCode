import java.io.File

fun main() {
    val input = parseInput("2018/day04/sami286/input")
    println("part1: " + part1(input))
    println("part2: " + part2(input))
}

fun parseInput(input: String): Map<Int, IntArray> {
    val list = mutableMapOf<Int, IntArray>()
    var sleepMin = 0
    var id = 0

    File(input).readLines().sortedBy { it }.forEach {
        val min = it.substring(15, 17).toInt()
        val state = it.substring(19)

        when (state) {
            "falls asleep" -> {
                sleepMin = min
            }
            "wakes up" -> {
                for (i in sleepMin until min)
                    list[id]!![i]++
            }
            else -> {
                val tmp = state.substring(7)
                id = tmp.substring(0, tmp.indexOf(" ")).toInt()
                list.putIfAbsent(id, IntArray(60))
            }
        }
    }
    return list
}

fun part1(list: Map<Int, IntArray>): Int {
    var maxGuardID = 0
    var maxGuardSleep = 0
    list.forEach { guardId, time ->
        val total = time.sum()
        if (total > maxGuardSleep) {
            maxGuardID = guardId
            maxGuardSleep = total
        }
    }

    val tmp = list[maxGuardID]!!
    return tmp.indexOf(tmp.max()!!) * maxGuardID
}

fun part2(list: Map<Int, IntArray>): Int {
    var idMax = 0
    var minMax = 0
    var minID = 0
    list.forEach { guardId, time ->
        val max = time.max()!!
        if (max > minMax) {
            idMax = guardId
            minMax = max
            minID = time.indexOf(max)
        }
    }
    return idMax * minID
}