import Input.ListOps

object Advent16 {
  type Ticket = List[Int]
  case class Field(name: String, bounds: List[(Int, Int)])

  def main(args: Array[String]): Unit = {
    val (fields, ticket, otherTickets) = Input.byExercise(16).separatedBy(_.isEmpty).take(3) match {
      case rawFields :: myTicket :: nearbyTickets :: Nil =>
        (
          rawFields.map(toField),
          toTicket(myTicket.drop(1).head),
          nearbyTickets.drop(1).map(toTicket)
        )
    }

    val solution1 = otherTickets.flatMap(invalidField(_, fields))
    println(s"Solution1: ${solution1.sum}")

    val validTickets = ticket :: otherTickets.filter(ticket => invalidField(ticket, fields).isEmpty)

    val fieldsToIndex = fields
      .map { field =>
        field.name -> ticket.indices.filter { index =>
          validTickets.forall { ticket => validForField(ticket(index), field) }
        }
      }
      .sortBy(_._2.length)
      .foldLeft(Map.empty[String, Int]) { case (mapping, (fieldName, fieldIndices)) =>
        val usedIndices = mapping.values
        val possibleIndices = fieldIndices.toSet.removedAll(usedIndices)
        val fieldIndex = possibleIndices.head

        mapping.updated(fieldName, fieldIndex)
      }
      .filter(_._1.startsWith("departure"))

    val solution2 = fieldsToIndex.map { case (_, index) => ticket(index).toLong }.product
    println(s"Solution2: $solution2")
  }

  // departure location: 49 - 258 or 268 - 960
  private val fieldRegex = raw"(.+): (\d+)-(\d+) or (\d+)-(\d+)".r

  def toTicket(in: String): Ticket = in.split(',').map(_.toInt).toList

  def toField(in: String): Field = in match {
    case fieldRegex(name, a, b, c, d) =>
      Field(name, (a.toInt, b.toInt) :: (c.toInt, d.toInt) :: Nil)
  }

  def invalidField(ticket: Ticket, fields: List[Field]): Option[Int] =
    ticket.find { ticketNumber =>
      !fields.exists { field =>
        validForField(ticketNumber, field)
      }
    }

  def validForField(ticketNumber: Int, field: Field): Boolean =
    field.bounds.exists { case (lower, upper) =>
      lower <= ticketNumber && ticketNumber <= upper
    }
}
