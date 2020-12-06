import scala.annotation.tailrec

object Input {
  def byExercise(exercise: Int): List[String] = {
    val source = io.Source.fromFile(s"../inputs/$exercise")
    val lines = source.getLines().toList
    source.close()

    lines
  }

  implicit class ListOps[A](val input: List[A]) extends AnyVal {
    def separatedBy(p: A => Boolean): List[List[A]] = {

      @tailrec
      def go(input: List[A], result: List[List[A]]): List[List[A]] = {
        input.span(!p(_)) match {
          case (Nil, Nil)    => Nil :: result
          case (Nil, rest)   => go(rest.tail, Nil :: result)
          case (group, Nil)  => group :: result
          case (group, rest) => go(rest.tail, group :: result)
        }
      }

      go(input, Nil).reverse
    }
  }
}
