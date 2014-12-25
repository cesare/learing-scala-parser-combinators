import scala.util.parsing.combinator._


object samples {
  object PostalCodeParser extends RegexParsers {
    def postalCode = """\d{3}""".r ~ "-" ~ """\d{4}""".r
    def apply(input: String) = parseAll(postalCode, input).get
  }

  println(PostalCodeParser("123-4567"))           //> ((123~-)~4567)
}