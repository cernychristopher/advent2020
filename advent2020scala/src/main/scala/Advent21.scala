import scala.annotation.tailrec

object Advent21 {

  type Ingredient = String
  type Allergen = String
  case class Food(ingredients: Seq[Ingredient], allergens: Seq[Allergen])

  def main(args: Array[String]): Unit = {
    val inputs = Input.byExercise(21).map(toFood)

    val allIngredients = inputs.flatMap(_.ingredients).distinct

    val ingredientToAllergen = computeIngredients(
      inputs
        .flatMap { food =>
          food.allergens.map { _ -> food.ingredients }
        }
        .groupBy(_._1)
        .view
        .mapValues(_.map(_._2))
        .mapValues(_.reduce(_ intersect _))
        .toSeq
    )

    val allergicIngredients = ingredientToAllergen.keySet

    val harmlessIngredients = allIngredients.toSet.diff(allergicIngredients)

    val solution1 = inputs.map(_.ingredients.count(harmlessIngredients.contains)).sum

    println(s"Solution1: $solution1")

    val solution2 = ingredientToAllergen.toSeq.sortBy(_._2).map(_._1).mkString(",")

    println(s"Solution2: $solution2")
  }

  private val foodRegex = raw"(.+) \(contains (.+)\)".r
  def toFood(in: String): Food = in match {
    case foodRegex(ingredients, allergens) =>
      Food(ingredients.split(' ').map(_.trim), allergens.split(',').map(_.trim))
  }

  def computeIngredients(
    allergensToIngredients: Seq[(Allergen, Seq[Ingredient])]
  ): Map[Ingredient, Allergen] = {
    @tailrec
    def go(
      acc: Map[Ingredient, Allergen],
      allergens: Seq[(Allergen, Seq[Ingredient])]
    ): Map[Ingredient, Allergen] = {
      val newMappings = allergens
        .filter(_._2.size == 1)
        .map { case (allergen, ingredients) =>
          ingredients.head -> allergen
        }
        .toMap

      val newClassifiedIngredients = newMappings.keySet

      val newAllergens = allergens
        .map { case (allergen, ingredients) =>
          allergen -> ingredients.toSet.diff(newClassifiedIngredients).toSeq
        }
        .filter(_._2.nonEmpty)

      val mappings = acc ++ newMappings
      if (newAllergens.isEmpty) mappings
      else go(mappings, newAllergens)
    }

    go(Map.empty, allergensToIngredients)
  }
}
