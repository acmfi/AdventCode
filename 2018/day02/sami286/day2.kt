import java.io.File

fun main() {
    println("part1: " + part1("2018/day02/sami286/input"))
    println("part2: " + part2("2018/day02/sami286/input"))
}

fun part1(input: String): Int {
    var twoTimes = 0
    var threeTimes = 0

    File(input).forEachLine { line ->
        var lineTmp = line
        var two = false
        var three = false

        while (lineTmp.isNotBlank()) {
            if (two && three) break
            when (lineTmp.count { it == lineTmp[0] }) {
                2 -> {
                    if (!two)
                        twoTimes++
                    two = true
                }
                3 -> {
                    if (!three)
                        threeTimes++
                    three = true
                }
            }
            lineTmp = lineTmp.replace(lineTmp[0].toString(), "")
        }
    }
    return twoTimes * threeTimes
}

fun part2(input: String): String? {
    val lines = File(input).readLines()
    lines.forEachIndexed { index, line ->
        for (i in index + 1 until lines.size) {
            if (diff(line, lines[i]) == 1) {
                return clean(line, lines[i])
            }
        }
    }
    return null
}

fun diff(s1: String, s2: String): Int {
    var dif = 0
    for (i in 0 until s1.length)
        if (s1[i] != s2[i])
            dif++
    return dif
}

fun clean(s1: String, s2: String): String {
    for (i in 0 until s1.length)
        if (s1[i] != s2[i]) {
            val tmp = StringBuilder(s1)
            tmp.deleteCharAt(i)
            return tmp.toString()
        }
    return "Error"
}