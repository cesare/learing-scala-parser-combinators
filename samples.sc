import scala.util.parsing.combinator._

// Trying http://qiita.com/suin/items/35bc4afe618cb77f80f6

object samples {
  case class PostalCode(threeDigit: String, fourDigit: String)

  object PostalCodeParser extends RegexParsers {
    def postalCode = """\d{3}""".r ~ "-" ~ """\d{4}""".r ^^ {
      case (threeDigit ~ "-" ~ fourDigit) => PostalCode(threeDigit, fourDigit)
    }
    def apply(input: String): Either[String, Any] = parseAll(postalCode, input) match {
      case Success(result, next) => Right(result)
      case NoSuccess(error, next) => Left(error)
    }
  }

  println(PostalCodeParser("123-4567"))           //> Right(PostalCode(123,4567))
  println(PostalCodeParser("1234567890"))         //> Left(`-' expected but `4' found)
}