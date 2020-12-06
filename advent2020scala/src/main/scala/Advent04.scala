import Input.ListOps

object Advent04 {
  def main(args: Array[String]): Unit = {
    val passports = Input.byExercise(4).separatedBy(_.isEmpty).map(toPassport)

    val completePassportCount = passports.count(passportHasAllFields)
    val validPassportCount = passports.count(validatePassport)

    println(s"Solution1: $completePassportCount")
    println(s"Solution2: $validPassportCount")
  }

  type Passport = Map[String, String]

  val requiredFields = List("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")
  val eyeColors = List("amb", "blu", "brn", "gry", "grn", "hzl", "oth")

  def toPassport(passportLines: Iterable[String]): Passport = passportLines
    .flatMap(_.split(' '))
    .filter(_.contains(':'))
    .map(_.split(":", 2).toList)
    .collect { case key :: value :: Nil =>
      (key.trim, value.trim)
    }
    .toMap

  def passportHasAllFields(passport: Passport): Boolean =
    requiredFields.forall(passport.contains)

  def validateYear(min: Int, max: Int)(year: String): Boolean =
    year.matches("\\d{4}") && year.toInt >= min && year.toInt <= max

  def validateHeight(hgt: String): Boolean = {
    if (hgt.endsWith("in")) {
      val inches = hgt.dropRight(2).toInt
      inches >= 59 && inches <= 76
    } else if (hgt.endsWith("cm")) {
      val centimeters = hgt.dropRight(2).toInt
      centimeters >= 150 && centimeters <= 193
    } else {
      false
    }
  }

  val fieldValidators: List[(String, String => Boolean)] = List(
    "byr" -> validateYear(1920, 2002),
    "iyr" -> validateYear(2010, 2020),
    "eyr" -> validateYear(2020, 2030),
    "hgt" -> validateHeight,
    "hcl" -> "#[0-9a-f]{6}".r.matches,
    "ecl" -> eyeColors.contains,
    "pid" -> "\\d{9}".r.matches
  )

  def validatePassport(passport: Passport): Boolean =
    passportHasAllFields(passport) &&
      fieldValidators.forall { case (key, validator) => passport.get(key).exists(validator) }
}
