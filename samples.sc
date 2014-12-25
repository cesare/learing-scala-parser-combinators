import scala.util.parsing.combinator._


object samples {
  object PostalCodeParser extends RegexParsers {
    def postalCode = """\d{3}""".r ~ "-" ~ """\d{4}""".r
  }

  println(PostalCodeParser.parseAll(PostalCodeParser.postalCode, "123-4567").get)
                                                  //> ((123~-)~4567)
}