import Input.ListOps

import scala.annotation.tailrec

object Advent18 {

  sealed trait Operator
  case object Plus extends Operator
  case object Times extends Operator

  sealed trait Expression
  case class Number(value: Long) extends Expression
  case class Calculation(operators: List[Operator], operands: List[Expression]) extends Expression

  def main(args: Array[String]): Unit = {
    val input = Input
      .byExercise(18)

    val expressions: List[Expression] =
      input
        .map { _.replace(" ", "") }
        .map(parseExpression)

    val plusFirst: List[Expression] =
      expressions.map(toPlusFirst) //transform the tree so that '+' is evaluated before '*'

    val solution1 = expressions.map(evaluate)
    val solution2 = plusFirst.map(evaluate)

    println(s"Solution1: ${solution1.sum}")
    println(s"Solution2: ${solution2.sum}")
  }

  private def toOperator(operatorChar: Char) = operatorChar match {
    case '+' => Plus
    case '*' => Times
  }

  private val simpleNumberRegex = raw"(\d)([+*]).*".r
  private val justNumberRegex = raw"(\d)$$".r

  def parseExpression(expression: String): Expression = {
    @tailrec
    def parseSubExpression(
      expression: String,
      operators: List[Operator],
      operands: List[Expression]
    ): Expression = {
      expression match {
        case justNumberRegex(number) => Calculation(operators, operands :+ Number(number.toLong))
        case simpleNumberRegex(number, operator) =>
          parseSubExpression(
            expression.drop(2),
            operators :+ toOperator(operator.charAt(0)),
            operands :+ Number(number.toLong)
          )
        case _ =>
          val (left, right) = findMatchingParentheses(expression)

          if (right.isEmpty)
            Calculation(operators, operands :+ parseExpression(left))
          else {
            parseSubExpression(
              right.drop(1),
              operators :+ toOperator(right.charAt(0)),
              operands :+ parseExpression(left)
            )
          }
      }
    }

    parseSubExpression(expression, Nil, Nil)
  }

  def findMatchingParentheses(expression: String): (String, String) = {
    var open = 1
    val (found, rest) = expression.drop(1).span { char =>
      if (char == '(') open = open + 1
      if (char == ')') open = open - 1

      open != 0
    }

    (found, rest.drop(1))
  }

  // evaluate expressions left to right
  def evaluate(expression: Expression): Long = expression match {
    case Number(value) => value
    case Calculation(operators, firstOperand :: otherOperands) =>
      operators.zip(otherOperands).foldLeft(evaluate(firstOperand)) {
        case (acc, (operator, operand)) =>
          val nextValue = evaluate(operand)
          operator match {
            case Plus  => acc + nextValue
            case Times => acc * nextValue
          }
      }
  }

  private val plusses = LazyList.continually(Plus)
  private val stars = LazyList.continually(Times)

  def toPlusFirst(expression: Expression): Expression = expression match {
    case number: Number => number
    case Calculation(operators, operands) =>
      val factors = operators
        .separatedBy(_ == Times)
        .map(_.length + 1)
        .foldLeft(operands.map(toPlusFirst) -> List.empty[List[Expression]]) {
          case ((remaining, collected), count) =>
            remaining.drop(count) -> (collected :+ remaining.take(count))
        }
        ._2
        .map { summands => Calculation(plusses.take(summands.length - 1).toList, summands) }

      Calculation(stars.take(factors.length - 1).toList, factors)
  }
}
