object Advent04 {
  val requiredFields = List("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")
  val eyeColors = List("amb", "blu", "brn", "gry", "grn", "hzl", "oth")

  def nextPassport(inputs: List[String]): (List[String], Map[String, String]) = {
    val passportLines = inputs.takeWhile(_.nonEmpty)

    val passPort = passportLines
      .flatMap(_.split(' '))
      .filter(_.contains(':'))
      .map(_.split(":", 2).toList)
      .collect { case key :: value :: Nil =>
        (key.trim, value.trim)
      }
      .toMap

    (inputs.dropWhile(_.nonEmpty).dropWhile(_.isEmpty), passPort)
  }

  def getPassports(inputs: List[String]): List[Map[String, String]] = {
    val (rest, passPort) = nextPassport(inputs)

    rest match {
      case Nil => List(passPort)
      case _   => passPort :: getPassports(rest)
    }
  }

  def passPortHasAllFields(passport: Map[String, String]): Boolean =
    requiredFields.forall(requiredField => passport.contains(requiredField))

  def validatePassport(passport: Map[String, String]): Boolean =
    passPortHasAllFields(passport) &&
      passport.get("byr").exists { byr =>
        byr.matches("\\d{4}") && byr.toInt >= 1920 && byr.toInt <= 2002
      } &&
      passport.get("iyr").exists { iyr =>
        iyr.matches("\\d{4}") && iyr.toInt >= 2010 && iyr.toInt <= 2020
      } &&
      passport.get("eyr").exists { eyr =>
        eyr.matches("\\d{4}") && eyr.toInt >= 2020 && eyr.toInt <= 2030
      } && passport.get("hgt").exists { hgt =>
        if (hgt.endsWith("in")) {
          val inches = hgt.dropRight(2).toInt
          inches >= 59 && inches <= 76
        } else if (hgt.endsWith("cm")) {
          val centimeters = hgt.dropRight(2).toInt
          centimeters >= 150 && centimeters <= 193
        } else {
          false
        }
      } && passport.get("hcl").exists { hcl => "^#[0-9a-f]{6}$".r.matches(hcl) } && passport
        .get("ecl")
        .exists(eyeColors.contains) && passport.get("pid").exists(pid => "\\d{9}".r.matches(pid))

  def main(args: Array[String]): Unit = {
    val inputs = Input.byExercise(4)

    val passports = getPassports(inputs)

    passports.foreach(println)

    val completePassportCount = passports.count(passPortHasAllFields)
    val validPassportCount = passports.count(validatePassport)

    println(s"Soltution1: $completePassportCount")
    println(s"Soltution2: $validPassportCount")
  }
}
