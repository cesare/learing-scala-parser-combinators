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
      case NoSuccess(error, next) => Left(s"${error} on line ${next.pos.line}, column ${next.pos.column}")
    }
  }

  println(PostalCodeParser("123-4567"))           //> Right(PostalCode(123,4567))
  println(PostalCodeParser("1234567890"))         //> Left(`-' expected but `4' found on line 1, column 4)

  /*
   * CSV parser
   */
  case class CSV(header: Header, rows: Seq[Record])
  case class Header(names: Seq[String])
  case class Record(fields: Seq[String])

  object CSVParser extends RegexParsers {
    override def skipWhitespace = false

    def file = opt(header <~ CRLF) ~ repsep(record, CRLF) <~ opt(CRLF) ^^ {
      case Some(header) ~ records => CSV(header, records)
      case None ~ records => CSV(Header(List()), records)
    }

    def header = repsep(name, comma) ^^ { names =>
      Header(names.map(_.toString))
    }

    def record = repsep(field, comma) ^^ { fields =>
      Record(fields.map(_.toString))
    }

    def name = field
    def field = escaped | nonEscaped

    def escaped = doubleQuote ~> (textdata | comma | CR | LF | twoDoubleQuotes).* <~ doubleQuote ^^ { _.mkString }
    def nonEscaped = textdata.* ^^ { _.mkString }

    def textdata = """[\u0020-\u0021\u0023-\u002B\u002D-\u007E]""".r

    def doubleQuote = "\""
    def twoDoubleQuotes = doubleQuote ~ doubleQuote ^^ { case l ~ r => l }

    def comma = ","

    def CR = "\r"
    def LF = "\n"
    def CRLF = CR ~ LF

    def apply(input: String): Either[String, Any] = parseAll(file, input) match {
      case Success(results, next) => Right(results)
      case NoSuccess(error, next) => Left(s"${error} on line ${next.pos.line}, column ${next.pos.column}")
    }
  }

  CSVParser("a,b,c\r\n1,2,3\r\n\"x\"\"\",y,z")    //> res0: Either[String,Any] = Right(CSV(Header(List(a, b, c)),List(Record(List
                                                  //| (1, 2, 3)), Record(List(x", y, z)))))
}