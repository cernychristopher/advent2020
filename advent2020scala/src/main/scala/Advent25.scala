object Advent25 {
  def main(args: Array[String]): Unit = {
    val subjectNumber: Long = 7

    val publicKeys = LazyList.iterate(1L) { _ * subjectNumber % 20201227 }

    val cardPublicKey = 14222596
    val doorPublicKey = 4057428

    val cardLoopSize = publicKeys.indexOf(cardPublicKey)
    val doorLoopSize = publicKeys.indexOf(doorPublicKey)

    val encryptionKey =
      Iterator.iterate(1L) { _ * doorPublicKey % 20201227 }.drop(cardLoopSize).next()
    /*
    val encryptionKey2 =
      Iterator.iterate(1L) { _ * cardPublicKey % 20201227 }.drop(doorLoopSize).next()*/

    val solution1 = encryptionKey
    println(s"Solution1: $solution1")
  }
}
