object Advent07 {

  type Color = String
  case class Rule(outerColor: Color, amount: Int, innerColor: Color)

  def main(args: Array[String]): Unit = {
    val rules = Input.byExercise(7).flatMap(toBagRules)

    val solution1 = LazyList
      .iterate(Set("shiny gold")) { containedBy(rules, _) }
      .drop(1)
      .takeWhile(_.nonEmpty)
      .take(100) // just making we are not forcing an infinite List! There might be circles!
      .reduce(_ ++ _)
      .size

    println(s"Solution1: $solution1")
    println(s"Solution2: ${countInnerBags(rules, "shiny gold")}")
  }

  def containedBy(rules: List[Rule], innerColors: Set[Color]): Set[Color] =
    rules.filter(rule => innerColors.contains(rule.innerColor)).map(_.outerColor).toSet

  def countInnerBags(rules: List[Rule], startingColor: Color): Int = {
    val groupedRules = rules.groupBy(_.outerColor)

    def countTree(color: Color): Int = {
      1 + groupedRules
        .getOrElse(color, Nil)
        .map { rule =>
          if (rule.amount == 0) 0
          else countTree(rule.innerColor) * rule.amount
        }
        .sum
    }

    countTree(
      startingColor
    ) - 1 // do not count the outermost bag, that is colored in "startingColor"
  }

  // muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
  def toBagRules(in: String): List[Rule] = {
    val regex = raw"(.+) bags contain (.+)\.".r
    in match {
      case regex(outerColor, rest) =>
        rest.split(',').map(_.trim).map(toBagRule(outerColor, _)).toList
    }
  }

  //1 shiny gold bag.
  def toBagRule(outerColor: String, in: String): Rule = {
    val regex = raw"(\d+|no) (.+) bags?".r

    in match {
      case regex("no", color)  => Rule(outerColor, 0, color)
      case regex(count, color) => Rule(outerColor, count.toInt, color)
    }
  }
}
