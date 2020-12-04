object Advent04 {
  val requiredFields = List("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")
  val eyeColors = List("amb", "blu", "brn", "gry", "grn", "hzl", "oth")

  def nextPassport(inputs: List[String]): (List[String], Map[String, String]) = {
    val (passportLines, rest) = inputs.span(_.nonEmpty)

    val passport = passportLines
      .flatMap(_.split(' '))
      .filter(_.contains(':'))
      .map(_.split(":", 2).toList)
      .collect { case key :: value :: Nil =>
        (key.trim, value.trim)
      }
      .toMap

    (rest.dropWhile(_.isEmpty), passport)
  }

  def getPassports(inputs: List[String]): List[Map[String, String]] = {
    val (rest, passPort) = nextPassport(inputs)

    rest match {
      case Nil => List(passPort)
      case _   => passPort :: getPassports(rest)
    }
  }

  def passPortHasAllFields(passport: Map[String, String]): Boolean =
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

  def validatePassport(passport: Map[String, String]): Boolean =
    passPortHasAllFields(passport) &&
      passport.get("byr").exists(validateYear(1920, 2002)) &&
      passport.get("iyr").exists(validateYear(2010, 2020)) &&
      passport.get("eyr").exists(validateYear(2020, 2030)) &&
      passport.get("hgt").exists(validateHeight) &&
      passport.get("hcl").exists("#[0-9a-f]{6}".r.matches) &&
      passport.get("ecl").exists(eyeColors.contains) &&
      passport.get("pid").exists("\\d{9}".r.matches)

  def main(args: Array[String]): Unit = {
    val inputs = Input.byExercise(4)

    val passports = getPassports(inputs)

    val completePassportCount = passports.count(passPortHasAllFields)
    val validPassportCount = passports.count(validatePassport)

    println(s"Solution1: $completePassportCount")
    println(s"Solution2: $validPassportCount")
  }
}
