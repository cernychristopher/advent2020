object Advent12 {
  case class Instruction(direction: Char, amount: Int)
  case class ShipState(x: Int, y: Int, direction: Int)
  case class Position(x: Int, y: Int)
  case class WaypointState(ship: Position, waypoint: Position)

  def toInstruction(in: String): Instruction = {
    val char = in.head.toUpper
    val amount = in.drop(1).toInt

    Instruction(char, amount)
  }

  def main(args: Array[String]): Unit = {
    val input = Input.byExercise(12).map(toInstruction)

    val initialState = ShipState(0, 0, 0)

    val finalState = input.foldLeft(initialState) { (ship, instruction) =>
      val amount = instruction.amount

      instruction.direction match {
        case 'N' => ship.copy(y = ship.y - amount)
        case 'S' => ship.copy(y = ship.y + amount)
        case 'E' => ship.copy(x = ship.x + amount)
        case 'W' => ship.copy(x = ship.x - amount)
        case 'R' => ship.copy(direction = (ship.direction - amount + 360) % 360)
        case 'L' => ship.copy(direction = (ship.direction + amount + 360) % 360)
        case 'F' =>
          ship.direction match {
            case 0   => ship.copy(x = ship.x + amount)
            case 90  => ship.copy(y = ship.y - amount)
            case 180 => ship.copy(x = ship.x - amount)
            case 270 => ship.copy(y = ship.y + amount)
          }
      }
    }

    val solution1 = Math.abs(finalState.x) + Math.abs(finalState.y)

    println(s"Solution1: $solution1")

    val finalState2 =
      input
        .foldLeft(WaypointState(ship = Position(0, 0), waypoint = Position(10, -1))) {
          case (state, Instruction(direction, amount)) =>
            val WaypointState(ship, waypoint) = state

            direction match {
              case 'N' => state.copy(waypoint = waypoint.copy(y = waypoint.y - amount))
              case 'S' => state.copy(waypoint = waypoint.copy(y = waypoint.y + amount))
              case 'E' => state.copy(waypoint = waypoint.copy(x = waypoint.x + amount))
              case 'W' => state.copy(waypoint = waypoint.copy(x = waypoint.x - amount))
              case 'L' =>
                amount match {
                  case 0   => state
                  case 90  => state.copy(waypoint = waypoint.copy(x = waypoint.y, y = -waypoint.x))
                  case 180 => state.copy(waypoint = waypoint.copy(x = -waypoint.x, y = -waypoint.y))
                  case 270 => state.copy(waypoint = waypoint.copy(x = -waypoint.y, y = waypoint.x))
                }
              case 'R' =>
                amount match {
                  case 0   => state
                  case 90  => state.copy(waypoint = waypoint.copy(x = -waypoint.y, y = waypoint.x))
                  case 180 => state.copy(waypoint = waypoint.copy(x = -waypoint.x, y = -waypoint.y))
                  case 270 => state.copy(waypoint = waypoint.copy(x = waypoint.y, y = -waypoint.x))
                }
              case 'F' =>
                state.copy(ship =
                  ship.copy(x = ship.x + amount * waypoint.x, y = ship.y + amount * waypoint.y)
                )
            }
        }

    val solution2 = Math.abs(finalState2.ship.x) + Math.abs(finalState2.ship.y)

    println(s"Solution2: $solution2")
  }
}
