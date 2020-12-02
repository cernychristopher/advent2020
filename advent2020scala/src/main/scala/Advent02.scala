object Advent02 {
  case class PasswordPolicy(first: Int, second: Int, char: Char)

  def main(args: Array[String]): Unit = {
    val inputs = Input.byExercise(2)

    val regex = raw"(\d+)-(\d+) (.): (.+)".r

    val passwords = inputs.collect { case regex(min, max, char, password) =>
      PasswordPolicy(min.toInt, max.toInt, char.charAt(0)) -> password
    }

    // count the number of passwords that are valid according to the policy
    println(passwords.count { case (policy, password) =>
      val charCount = password.count(_ == policy.char)

      charCount >= policy.first && charCount <= policy.second
    })

    // oh no, the policy has changed! Exactly one character at the given positions must be equal to the character
    // stored in the password policy
    println(passwords.count { case (policy, password) =>
      val firstChar = password.charAt(policy.first - 1)
      val secondChar = password.charAt(policy.second - 1)

      (firstChar == policy.char) ^ (secondChar == policy.char)
    })
  }
}
