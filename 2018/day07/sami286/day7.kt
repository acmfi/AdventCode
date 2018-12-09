package day7

import java.io.File

fun main() {
    println("part1: " + part1(getSteps("src/day7/input")))
    println("part2: " + part2(getSteps("src/day7/input")))
}

fun getSteps(input: String): MutableMap<Char, MutableList<Char>> {
    val list = mutableMapOf<Char, MutableList<Char>>()
    File(input).readLines().forEach {
        val requirement = it[5]
        val step = it[36]
        list.putIfAbsent(step, mutableListOf())
        list.putIfAbsent(requirement, mutableListOf())
        list[step]!!.add(requirement)
    }
    return list
}

fun part1(steps: MutableMap<Char, MutableList<Char>>): StringBuilder {
    val order = StringBuilder()
    while (steps.isNotEmpty()) {
        val finish = steps.filter { it.value.isEmpty() }.map { it.key }.sorted()[0]
        order.append(finish)
        steps.remove(finish)
        steps.forEach { it.value.remove(finish) }
    }
    return order
}


fun part2(steps: MutableMap<Char, MutableList<Char>>): Int {
    val workers = Array(5) { Triple(it,'-', 0) }
    var time = 0
    while (workers.any { it.third>=0 }) {
        val readySteps = steps.filterValues { it.isEmpty() }.map { it.key }.sorted()
        val idleWorkers = workers.filter { it.third <= 0 }.map { workers.indexOf(it) }
        idleWorkers.forEachIndexed { i, iWorkers ->
            if (i >= readySteps.size)
                return@forEachIndexed
            workers[iWorkers] = Triple(iWorkers, readySteps[i], readySteps[i].toInt() - 4)
            steps.remove(readySteps[i])
        }
        for (i in workers.indices) {
            workers[i] = Triple(i, workers[i].second, workers[i].third - 1)
            if (workers[i].third == 0) {
                steps.forEach { it.value.remove(workers[i].second) }
            }
         }
        time++
    }
    return time-1
}