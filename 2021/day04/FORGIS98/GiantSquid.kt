package Kotlin

import java.io.File

fun readFile(fileName: String): List<String> = File(fileName).bufferedReader().readLines()

fun inputToMatrix(input: MutableList<String>): Array<Array<Array<Int>>> {

    var allBoards: Array<Array<Array<Int>>> = arrayOf<Array<Array<Int>>>()
    var matrix: Array<Array<Int>> = arrayOf<Array<Int>>()

    input.removeAt(0)
    input.removeAt(0)

    for (i in input) {
        if (i.equals("")) {
            allBoards += matrix
            matrix = arrayOf<Array<Int>>()
        } else {
            matrix += i.split(" ").map { it.toInt() }.toTypedArray()
        }
    }

    // No empty line at the end of input.txt
    allBoards += matrix
    return allBoards
}

fun checkNumber(board: Array<Array<Int>>, number: Int): Array<Array<Int>> {
    for (i in 0..board.size - 1) {
        board[i] = board[i].map { if (it == number) -1 else it }.toTypedArray()
    }

    return board
}

fun isWinner(board: Array<Array<Int>>): Boolean {

    // sum of row
    for (i in board) {
        if (i.sum() == -5) {
            return true
        }
    }

    // sum of columns
    var column: Int = 0
    for (i in 0..board.size - 1) {
        for (y in 0..board.size - 1) {
            column += board[y][i]
        }

        if (column == -5) {
            return true
        }

        column = 0
    }

    return false
}

fun countNonSelected(board: Array<Array<Int>>): Int {
    var nonSelected: Int = 0

    for (i in board) {
        nonSelected += i.filter { it != -1 }.sum()
    }

    return nonSelected
}

fun printBoardsStatus(allBoards: Array<Array<Array<Int>>>) {
    for (i in allBoards) {
        for (y in i) {
            for (it in y) {
                print("${it} ")
            }
            print("\n")
        }
        print("\n")
    }
}

fun main() {

    // format input with vim commands ":%s/  / /g" and ":%s/^ //g"
    val myFile: List<String> = readFile("/home/jorge/input.txt")
    val bingoNumbers: List<Int> = myFile.get(0).split(",").map { it.toInt() }
    var allBoards: Array<Array<Array<Int>>> = inputToMatrix(myFile.toMutableList())

    var bingoNumber: Int = 0
    var nonSelected: Int = 0
    var stopTheMadness: Boolean = false

    for (number in bingoNumbers) {
        for (i in 0..allBoards.size - 1) {
            allBoards[i] = checkNumber(allBoards[i], number)
            if (isWinner(allBoards[i])) {
                // printBoardsStatus(allBoards)
                nonSelected = countNonSelected(allBoards[i])
                bingoNumber = number
                stopTheMadness = true
                break
            }
        }

        if (stopTheMadness) {
            break
        }
    }

    println("First Star: ${bingoNumber * nonSelected}")
}
