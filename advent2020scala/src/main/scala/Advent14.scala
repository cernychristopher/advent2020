object Advent14 {
  sealed trait Instruction
  case class SetMask(mask: String) extends Instruction
  case class StoreValue(address: Long, value: Long) extends Instruction

  case class ProgramState(memory: Map[Long, Long], andMask: Long, orMask: Long)
  case class ProgramState2(memory: Map[Long, Long], mask: String)

  val storeFormat = raw"mem\[(\d+)\] = (\d+)".r
  val maskFormat = raw"mask = ([01X]{36})".r

  def toInstruction(in: String): Instruction = in match {
    case maskFormat(mask)            => SetMask(mask)
    case storeFormat(address, value) => StoreValue(address.toLong, value.toLong)
  }

  def main(args: Array[String]): Unit = {
    val input = Input.byExercise(14).map(toInstruction)

    val computation = input.foldLeft(ProgramState(Map(), andMask = 0xfffffffffL, orMask = 0)) {
      (state, instruction) =>
        instruction match {
          case StoreValue(address, value) =>
            state.copy(memory =
              state.memory.updated(address, (value & state.andMask) | state.orMask)
            )
          case SetMask(mask) =>
            state.copy(
              andMask = java.lang.Long.parseLong(mask.replace('X', '1'), 2),
              orMask = java.lang.Long.parseLong(mask.replace('X', '0'), 2)
            )
        }
    }

    val solution1 = computation.memory.values.sum

    println(s"Solution1: $solution1")

    val computation2 = input.foldLeft(ProgramState2(Map(), mask = "")) { (state, instruction) =>
      instruction match {
        case StoreValue(address, value) =>
          val mask = state.mask.toList.reverse
          val addresses = computeAndMasks(mask).zip(computeOrMasks(mask)).map { case (and, or) =>
            (address & and) | or
          }

          val newMemory = addresses.foldLeft(state.memory) { (memory, address) =>
            memory.updated(address, value)
          }

          state.copy(memory = newMemory)
        case SetMask(mask) =>
          state.copy(mask = mask)
      }
    }

    val solution2 = computation2.memory.values.sum

    println(s"Solution2: $solution2")
  }

  // lsb to msb
  def computeOrMasks(chars: List[Char]): List[Long] = chars match {
    case Nil => List(0)
    case 'X' :: tail =>
      val results = computeOrMasks(tail)
      results.map(_ << 1) ++ results.map(n => (n << 1) + 1)
    case '0' :: tail => computeOrMasks(tail).map(_ << 1)
    case '1' :: tail => computeOrMasks(tail).map(n => (n << 1) + 1)
  }

  def computeAndMasks(chars: List[Char]): List[Long] = chars match {
    case Nil => List(0)
    case 'X' :: tail =>
      val results = computeAndMasks(tail)
      results.map(_ << 1) ++ results.map(n => (n << 1) + 1)
    case _ :: tail => computeAndMasks(tail).map(n => (n << 1) + 1)
  }
}
