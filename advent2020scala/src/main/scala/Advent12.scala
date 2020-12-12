object Advent12 {
  sealed trait Instruction
  case class Forward(amount: Int) extends Instruction
  case class Turn(degrees: Int) extends Instruction
  case class Move(x: Int, y: Int) extends Instruction

  case class ShipState(x: Int, y: Int, direction: Int)
  case class Position(x: Int, y: Int)
  case class WaypointState(ship: Position, waypoint: Position)

  /*
   The coordinates start in the bottom left corner (like in the math class)
   The y axis describes N-S movement (north, -south)
   The x axis is W-E movement (+E, -W)
   */
  def toInstruction(in: String): Instruction = {
    val amount = in.drop(1).toInt

    in.head.toUpper match {
      case 'N' => Move(0, amount)
      case 'S' => Move(0, -amount)
      case 'E' => Move(amount, 0)
      case 'W' => Move(-amount, 0)
      case 'R' => Turn(normalizeDirection(-amount))
      case 'L' => Turn(normalizeDirection(amount))
      case 'F' => Forward(amount)
    }
  }

  // direction should be always 0..359
  private def normalizeDirection(degrees: Int) = (degrees + 360) % 360

  def main(args: Array[String]): Unit = {
    val input = Input.byExercise(12).map(toInstruction)

    val initialState = ShipState(0, 0, 0)

    val finalState = input.foldLeft(initialState) { (ship, instruction) =>
      instruction match {
        case Move(x, y)    => ship.copy(x = ship.x + x, y = ship.y + y)
        case Turn(degrees) => ship.copy(direction = normalizeDirection(ship.direction + degrees))
        case Forward(amount) =>
          ship.direction match {
            case 0   => ship.copy(x = ship.x + amount)
            case 90  => ship.copy(y = ship.y + amount)
            case 180 => ship.copy(x = ship.x - amount)
            case 270 => ship.copy(y = ship.y - amount)
          }
      }
    }

    val solution1 = Math.abs(finalState.x) + Math.abs(finalState.y)

    println(s"Solution1: $solution1")

    val finalState2 =
      input
        .foldLeft(WaypointState(ship = Position(0, 0), waypoint = Position(10, 1))) {
          case (WaypointState(ship, waypoint), instruction) =>
            val newWaypoint = instruction match {
              case Move(x, y) =>
                waypoint.copy(x = waypoint.x + x, y = waypoint.y + y)
              case Turn(amount) =>
                amount match {
                  case 0   => waypoint
                  case 90  => waypoint.copy(x = -waypoint.y, y = waypoint.x)
                  case 180 => waypoint.copy(x = -waypoint.x, y = -waypoint.y)
                  case 270 => waypoint.copy(x = waypoint.y, y = -waypoint.x)
                }
              case _ => waypoint
            }

            val newShip = instruction match {
              case Forward(amount) =>
                ship.copy(x = ship.x + amount * waypoint.x, y = ship.y + amount * waypoint.y)
              case _ => ship
            }

            WaypointState(newShip, newWaypoint)
        }

    val solution2 = Math.abs(finalState2.ship.x) + Math.abs(finalState2.ship.y)

    println(s"Solution2: $solution2")
  }
}
