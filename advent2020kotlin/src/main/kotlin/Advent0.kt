import java.io.File

object Advent0 {
    fun input():List<String> = File("../inputs/0").readLines()
}

fun main() {
    println(
        Advent0.input()
            .map { it.toInt() }
            .sum()
    )
}
