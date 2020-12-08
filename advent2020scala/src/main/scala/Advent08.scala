object Advent08 {
  sealed trait Instruction
  case class Nop(amount: Int) extends Instruction
  case class Acc(amount: Int) extends Instruction
  case class Jump(amount: Int) extends Instruction

  sealed trait RunState
  case object Running extends RunState
  case object Terminated extends RunState
  case object Looping extends RunState
  case object Crashed extends RunState

  case class ProgramState(acc: Long, pc: Int, runState: RunState, visited: Set[Int])

  def main(args: Array[String]): Unit = {
    val program = Input.byExercise(8).map(toInstruction).toArray

    val solution1 = run(program).takeWhile { _.runState == Running }.last

    val modifiedPrograms = program.indices.flatMap(modifyProgram(program, _))

    val solution2 = modifiedPrograms
      .map(run)
      .map(_.take(200)) // determined through experimentation
      .find { execution =>
        execution.exists(_.runState == Terminated)
      }
      .map(_.toList.last)

    println(s"Solution1: $solution1")
    println(s"Solution2: $solution2")
  }

  def run(program: Array[Instruction]): Seq[ProgramState] =
    LazyList.iterate(ProgramState(0, 0, Running, Set.empty)) { state =>
      if (state.pc == program.length) state.copy(runState = Terminated)
      else if (state.pc < 0 || state.pc > program.length) state.copy(runState = Crashed)
      else {
        val runState = if (state.visited.contains(state.pc)) Looping else Running
        val visited = state.visited + state.pc

        program(state.pc) match {
          case Nop(_) => state.copy(pc = state.pc + 1, visited = visited, runState = runState)
          case Acc(amount) =>
            state.copy(
              acc = state.acc + amount,
              pc = state.pc + 1,
              visited = visited,
              runState = runState
            )
          case Jump(offset) =>
            state.copy(pc = state.pc + offset, visited = visited, runState = runState)
        }
      }
    }

  def modifyProgram(program: Array[Instruction], position: Int): Option[Array[Instruction]] =
    program(position) match {
      case Nop(amount)  => Option(program.updated(position, Jump(amount)))
      case Jump(offset) => Option(program.updated(position, Nop(offset)))
      case Acc(_)       => None
    }

  private val instructionRegex = raw"(...) ([+-]\d+)".r

  def toInstruction(code: String): Instruction = code match {
    case instructionRegex("jmp", amount) => Jump(amount.toInt)
    case instructionRegex("acc", amount) => Acc(amount.toInt)
    case instructionRegex("nop", amount) => Nop(amount.toInt)
  }
}
