object Input {
  def byExercise(exercise: Int): List[String] = {
    val source = io.Source.fromFile(s"../inputs/$exercise")
    val lines = source.getLines().toList
    source.close()

    lines
  }
}
