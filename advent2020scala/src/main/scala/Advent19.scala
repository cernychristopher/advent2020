import Input.ListOps

object Advent19 {
  type RuleId = Int

  sealed trait ParseRule {
    def id: RuleId
  }
  case class CharacterRule(id: RuleId, char: Char) extends ParseRule
  case class PositionRule(id: RuleId, subRules: List[List[RuleId]]) extends ParseRule

  def main(args: Array[String]): Unit = {
    val (messages, rules) = Input.byExercise(19).separatedBy(_.isEmpty) match {
      case r :: i :: Nil => i -> r.map(toRule)
    }

    val rulesById = rules.map { rule => rule.id -> rule }.toMap

    val validMessages =
      messages.intersect(allValidMessages(rulesById(0), rulesById).toList)
    val solution1 = validMessages.size

    val fortyTwos = LazyList.continually(42)
    val thirtyOnes = LazyList.continually(31)

    val newRule8 =
      PositionRule(8, (1 to 11).map(num => fortyTwos.take(num).toList).toList) // 8: 42 | 42 8
    val newRule11 = PositionRule(
      11,
      (1 to 11).map(num => fortyTwos.take(num).toList ::: thirtyOnes.take(num).toList).toList
    ) // 11: 42 31 | 42 11 31
    val patchedRules = rulesById.updated(8, newRule8).updated(11, newRule11)

    val strings31 = allValidMessages(rulesById(31), rulesById)
    val strings42 = allValidMessages(rulesById(42), rulesById)
    // 0: 8 11
    // -> any number of 42s, followed by m 42s and m 31s

    println(strings31.intersect(strings42)) // they are disjoint!!!
    println("---")

    val solution2 = messages.count { message =>
      val groups = message
        .grouped(strings31.head.length)
        .toList

      val matches42 = groups.takeWhile(strings42.contains).size
      val matches31 = groups.dropWhile(strings42.contains).takeWhile(strings31.contains).size

      message.length % strings31.head.length == 0 &&
      (matches31 + matches42) == groups.length &&
      matches31 > 0 &&
      matches42 > matches31
    }

    println(s"Solution1: $solution1")
    println(s"Solution2: $solution2")
  }
  /*
  0: 4 1 5
  1: 2 3 | 3 2
  2: 4 4 | 5 5
  3: 4 5 | 5 4
  4: "a"
  5: "b"
   */
  private val simpleRuleRegex = "\"(.)\"".r

  def toRule(in: String): ParseRule = {
    val stringId :: rest :: Nil = in.split(":").toList
    val id = stringId.toInt

    rest.trim match {
      case simpleRuleRegex(char) => CharacterRule(id, char.charAt(0))
      case numbers =>
        PositionRule(id, numbers.split('|').map(_.trim.split(' ').map(_.toInt).toList).toList)
    }
  }

  def allValidMessages(rule: ParseRule, rulesById: Map[RuleId, ParseRule]): Set[String] =
    rule match {
      case CharacterRule(_, char) => Set(char.toString)
      case PositionRule(_, subRules) =>
        subRules.map(allValidMessages(_, rulesById)).fold(Set.empty[String])(_ ++ _)
    }

  def allValidMessages(subRules: List[RuleId], rulesById: Map[RuleId, ParseRule]): Set[String] =
    subRules.map(rulesById).map(allValidMessages(_, rulesById)).foldLeft(Set("")) { (acc, set) =>
      for {
        a <- acc
        b <- set
      } yield a + b
    }
}
