import cats.syntax.either.*

import scala.io.Source

trait Challenge[TInput, TOut]:
  def getInputName: String
  def parse(s: Source): Either[Exception, List[TInput]]
  def process(in: List[TInput]): Either[Exception, TOut]

