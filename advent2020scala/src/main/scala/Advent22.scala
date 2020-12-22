import Input.ListOps

object Advent22 {
  type Deck = List[Int]

  def main(args: Array[String]): Unit = {
    val deck1 :: deck2 :: _ =
      Input.byExercise(22).separatedBy(_.isEmpty).map(_.tail).map(_.map(_.toInt)).take(2)

    // play a game of _combat_. It's over if one of the players does not have a deck any more
    val (finalDeck1, finalDeck2) = Iterator
      .iterate(deck1 -> deck2) {
        case (Nil, deck)                                   => Nil -> deck
        case (deck, Nil)                                   => deck -> Nil
        case (top1 :: rest1, top2 :: rest2) if top1 > top2 => (rest1 ::: List(top1, top2)) -> rest2
        case (top1 :: rest1, top2 :: rest2) if top2 > top1 => rest1 -> (rest2 ::: List(top2, top1))
      }
      .dropWhile(x => x._1.nonEmpty && x._2.nonEmpty)
      .next()

    val solution1 = computeScore(finalDeck1) + computeScore(finalDeck2)
    println(s"Solution1: $solution1")

    val (finalRecursiveDeck1, finalRecursiveDeck2) =
      playRecursiveGame(deck1, deck2, Set.empty, Set.empty)

    val solution2 = computeScore(finalRecursiveDeck1) + computeScore(finalRecursiveDeck2)

    println(s"Solution2: $solution2")
  }

  def computeScore(deck: Deck): Long = deck.reverse.zipWithIndex.map { case (card, index) =>
    card.toLong * (index + 1)
  }.sum

  def playRecursiveGame(
    deck1: Deck,
    deck2: Deck,
    previousDecks1: Set[Deck],
    previousDecks2: Set[Deck]
  ): (Deck, Deck) = {
    if (previousDecks1.contains(deck1) || previousDecks2.contains(deck2)) {
      // protection against infinite loops: if we arrive at a state that we have already seen, player 1 wins
      (deck1 ::: deck2, Nil)
    } else {
      val nextDecks1 = previousDecks1 + deck1
      val nextDecks2 = previousDecks2 + deck2

      (deck1, deck2) match {
        case (Nil, deck) => // player 1 lost the game
          Nil -> deck
        case (deck, Nil) => // player 2 lost the game
          deck -> Nil
        case (top1 :: rest1, top2 :: rest2) if top1 <= rest1.size && top2 <= rest2.size =>
          // play a sub-game that decides the outcome of this round
          playRecursiveGame(rest1.take(top1), rest2.take(top2), Set.empty, Set.empty) match {
            case (_, Nil) =>
              playRecursiveGame(rest1 ::: List(top1, top2), rest2, nextDecks1, nextDecks2)
            case (Nil, _) =>
              playRecursiveGame(rest1, rest2 ::: List(top2, top1), nextDecks1, nextDecks2)
          }
        // from here on it's just normal 'combat'
        case (top1 :: rest1, top2 :: rest2) if top1 > top2 => // player 1 won the round
          playRecursiveGame(rest1 ::: List(top1, top2), rest2, nextDecks1, nextDecks2)
        case (top1 :: rest1, top2 :: rest2) if top2 > top1 => // player 2 won the round
          playRecursiveGame(rest1, rest2 ::: List(top2, top1), nextDecks1, nextDecks2)
      }
    }
  }
}
