import Input.ListOps

object Advent06 {
  type Group = List[String]

  def personBits(line: String): Int =
    line.foldLeft[Int](0) { (bits, char) => bits | (1 << (char - 'a')) }

  def anyone(group: Group): Int = {
    val bits = group.foldLeft(0) { (bits, line) => bits | personBits(line) }

    Integer.bitCount(bits)
  }

  def everyone(group: Group): Int = {
    val bits = group.foldLeft(0x3ffffff) { (bits, line) => bits & personBits(line) }

    Integer.bitCount(bits)
  }

  def main(args: Array[String]): Unit = {
    val groups = Input.byExercise(6).separatedBy(_.isEmpty)

    println(s"Solution1: ${groups.map(anyone).sum}")
    println(s"Solution2: ${groups.map(everyone).sum}")
  }
}
