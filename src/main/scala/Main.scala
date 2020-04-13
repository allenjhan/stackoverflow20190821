import io.circe._
import io.circe.parser._
import io.circe.generic.semiauto._

object Main {

  def main(args: Array[String]) = {
    val testData =
      """
        |[{
        |    "id": 123,
        |    "latling": ["-12.777", "18.776"]
        |}, {
        |    "id": 123,
        |    "latling": [-12.777, 18.776]
        |}]
      """.stripMargin

    println(decode[List[Report]](testData))

    println(decode[List[DoubleReport]](testData))
  }

  case class Report(id: Int, latling: Either[List[String],List[Double]])

  object Report {
    implicit val reportDecoder: Decoder[Report] = new Decoder[Report] {
      override def apply(c: HCursor): Decoder.Result[Report] = {
        val stringAttempt = for {
          id <- c.downField("id").as[Int]
          latlingString <- c.downField("latling").as[List[String]]
        } yield Report(id, Left(latlingString))

        val doubleAttempt = for {
          id <- c.downField("id").as[Int]
          latlingDouble <- c.downField("latling").as[List[Double]]
        } yield Report(id, Right(latlingDouble))

        stringAttempt match {
          case Right(stringValue) => Right(stringValue)
          case Left(stringFailure) => doubleAttempt
        }
      }
    }
  }

  case class DoubleReport(id: Int, latling: List[Double])

  object DoubleReport {
    implicit val doubleReportDecoder: Decoder[DoubleReport] = deriveDecoder
  }
}
