import cats.effect.{ExitCode, IO, IOApp, Resource}
import cats.syntax.all.*

import scala.io.Source


object AoC extends IOApp {
  def run(args: List[String]): IO[ExitCode] = {
    //val challenge: Challenge[(Int, Int), Int] = Day1_1()
    val challenge: Challenge[(Int, Int), Int] = Day1_2()

    (for {
      f <- file(challenge.getInputName)
    } yield f
      ).use(source => challenge.parse(source).flatMap(challenge.process) match {
      case Left(error) => {
        Console.println(error.getMessage)
        IO.pure(ExitCode.Error)
      }
      case Right(result) => {
        Console.println(result)
        IO.pure(ExitCode.Success)
      }
    }
    )
  }

  def file(name: String): Resource[IO, Source] = Resource.make(sourceIO(name))(file => closeIO(file))


  private def sourceIO(name: String): IO[Source] = IO(Source.fromResource(name))

  private def closeIO(source: Source): IO[Unit] = IO(source.close())

  private def readLines(source: Source): IO[List[String]] = IO(source.getLines().toList)
}

