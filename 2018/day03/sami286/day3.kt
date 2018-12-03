import java.io.File

fun main(params: Array<String>) {
    println("part1: " + part1("2018/day03/sami286/input"))
    println("part2: " + part2("2018/day03/sami286/input"))
}

fun part1(input: String): Int {
    val tablero = Array(1000) { Array(1000) { 0 } }

    File(input).readLines().forEach {
        val x = it.substring(it.indexOf("@") + 2, it.indexOf(",")).toInt()
        val y = it.substring(it.indexOf(",") + 1, it.indexOf(":")).toInt()
        val w = it.substring(it.indexOf(":") + 2, it.indexOf("x")).toInt()
        val h = it.substring(it.indexOf("x") + 1).toInt()

        for (i in x until x + w) {
            for (j in y until y + h) {
                tablero[i][j]++
            }
        }
    }

    return tablero.flatten().filter { it > 1 }.size
}

fun part2(input: String): Int {
    val tablero = Array(1000) { IntArray(1000) }
    val noSolapa = mutableSetOf<Int>()

    File(input).readLines().forEach {
        val id = Integer.parseInt(it.substring(1, it.indexOf(" ")))
        val x = Integer.parseInt(it.substring(it.indexOf("@") + 2, it.indexOf(",")))
        val y = Integer.parseInt(it.substring(it.indexOf(",") + 1, it.indexOf(":")))
        val w = Integer.parseInt(it.substring(it.indexOf(":") + 2, it.indexOf("x")))
        val h = Integer.parseInt(it.substring(it.indexOf("x") + 1, it.length))
        noSolapa.add(id)

        for (i in x until x + w) {
            for (j in y until y + h) {
                if (tablero[i][j] == 0)
                    tablero[i][j] = id
                else if (tablero[i][j] > 0) {
                    noSolapa.remove(id)
                    noSolapa.remove(tablero[i][j])
                }
            }
        }

    }

    return noSolapa.iterator().next()
}